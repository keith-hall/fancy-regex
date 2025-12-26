// Copyright 2016 The Fancy Regex Authors.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! VM execution engine and state management.

use alloc::collections::BTreeSet;
use alloc::vec;
use alloc::vec::Vec;
use regex_automata::util::look::LookMatcher;
use regex_automata::util::primitives::NonMaxUsize;
use regex_automata::Anchored;
use regex_automata::Input;

use crate::error::RuntimeError;
use crate::prev_codepoint_ix;
use crate::{Assertion, Error, RegexOptions, Result};

use super::helpers::{
    codepoint_len_at, matches_literal, matches_literal_casei, store_capture_groups,
};
use super::{Delegate, Insn, Prog, MAX_STACK, OPTION_SKIPPED_EMPTY_MATCH, OPTION_TRACE};

#[cfg(feature = "variable-lookbehinds")]
use super::ReverseBackwardsDelegate;
#[derive(Debug)]
pub(super) struct Branch {
    pc: usize,
    ix: usize,
    nsave: usize,
}

#[derive(Debug)]
pub(super) struct Save {
    slot: usize,
    value: usize,
}

pub(crate) struct State {
    /// Saved values indexed by slot. Mostly indices to s, but can be repeat values etc.
    /// Always contains the saves of the current state.
    saves: Vec<usize>,
    /// Stack of backtrack branches.
    stack: Vec<Branch>,
    /// Old saves (slot, value)
    oldsave: Vec<Save>,
    /// Number of saves at the end of `oldsave` that need to be restored to `saves` on pop
    nsave: usize,
    explicit_sp: usize,
    /// Maximum size of the stack. If the size would be exceeded during execution, a `StackOverflow`
    /// error is raised.
    max_stack: usize,
    #[allow(dead_code)]
    options: u32,
}

// Each element in the stack conceptually represents the entire state
// of the machine: the pc (index into prog), the index into the
// string, and the entire vector of saves. However, copying the save
// vector on every push/pop would be inefficient, so instead we use a
// copy-on-write approach for each slot within the save vector. The
// top `nsave` elements in `oldsave` represent the delta from the
// current machine state to the top of stack.

impl State {
    fn new(n_saves: usize, max_stack: usize, options: u32) -> State {
        State {
            saves: vec![usize::MAX; n_saves],
            stack: Vec::new(),
            oldsave: Vec::new(),
            nsave: 0,
            explicit_sp: n_saves,
            max_stack,
            options,
        }
    }

    // push a backtrack branch
    fn push(&mut self, pc: usize, ix: usize) -> Result<()> {
        if self.stack.len() < self.max_stack {
            let nsave = self.nsave;
            self.stack.push(Branch { pc, ix, nsave });
            self.nsave = 0;
            self.trace_stack("push");
            Ok(())
        } else {
            Err(Error::RuntimeError(RuntimeError::StackOverflow))
        }
    }

    // pop a backtrack branch
    fn pop(&mut self) -> (usize, usize) {
        for _ in 0..self.nsave {
            let Save { slot, value } = self.oldsave.pop().unwrap();
            self.saves[slot] = value;
        }
        let Branch { pc, ix, nsave } = self.stack.pop().unwrap();
        self.nsave = nsave;
        self.trace_stack("pop");
        (pc, ix)
    }

    pub(super) fn save(&mut self, slot: usize, val: usize) {
        for i in 0..self.nsave {
            // could avoid this iteration with some overhead; worth it?
            if self.oldsave[self.oldsave.len() - i - 1].slot == slot {
                // already saved, just update
                self.saves[slot] = val;
                return;
            }
        }
        self.oldsave.push(Save {
            slot,
            value: self.saves[slot],
        });
        self.nsave += 1;
        self.saves[slot] = val;

        #[cfg(feature = "std")]
        if self.options & OPTION_TRACE != 0 {
            println!("saves: {:?}", self.saves);
        }
    }

    fn get(&self, slot: usize) -> usize {
        self.saves[slot]
    }

    // push a value onto the explicit stack; note: the entire contents of
    // the explicit stack is saved and restored on backtrack.
    fn stack_push(&mut self, val: usize) {
        if self.saves.len() == self.explicit_sp {
            self.saves.push(self.explicit_sp + 1);
        }
        let explicit_sp = self.explicit_sp;
        let sp = self.get(explicit_sp);
        if self.saves.len() == sp {
            self.saves.push(val);
        } else {
            self.save(sp, val);
        }
        self.save(explicit_sp, sp + 1);
    }

    // pop a value from the explicit stack
    fn stack_pop(&mut self) -> usize {
        let explicit_sp = self.explicit_sp;
        let sp = self.get(explicit_sp) - 1;
        let result = self.get(sp);
        self.save(explicit_sp, sp);
        result
    }

    /// Get the current number of backtrack branches
    fn backtrack_count(&self) -> usize {
        self.stack.len()
    }

    /// Discard backtrack branches that were pushed since the call to `backtrack_count`.
    ///
    /// What we want:
    /// * Keep the current `saves` as they are
    /// * Only keep `count` backtrack branches on `stack`, discard the rest
    /// * Keep the first `oldsave` for each slot, discard the rest (multiple pushes might have
    ///   happened with saves to the same slot)
    fn backtrack_cut(&mut self, count: usize) {
        if self.stack.len() == count {
            // no backtrack branches to discard, all good
            return;
        }
        // start and end indexes of old saves for the branch we're cutting to
        let (oldsave_start, oldsave_end) = {
            let mut end = self.oldsave.len() - self.nsave;
            for &Branch { nsave, .. } in &self.stack[count + 1..] {
                end -= nsave;
            }
            let start = end - self.stack[count].nsave;
            (start, end)
        };
        let mut saved = BTreeSet::new();
        // keep all the old saves of our branch (they're all for different slots)
        for &Save { slot, .. } in &self.oldsave[oldsave_start..oldsave_end] {
            saved.insert(slot);
        }
        let mut oldsave_ix = oldsave_end;
        // for other old saves, keep them only if they're for a slot that we haven't saved yet
        for ix in oldsave_end..self.oldsave.len() {
            let Save { slot, .. } = self.oldsave[ix];
            let new_slot = saved.insert(slot);
            if new_slot {
                // put the save we want to keep (ix) after the ones we already have (oldsave_ix)
                // note that it's fine if the indexes are the same (then swapping is a no-op)
                self.oldsave.swap(oldsave_ix, ix);
                oldsave_ix += 1;
            }
        }
        self.stack.truncate(count);
        self.oldsave.truncate(oldsave_ix);
        self.nsave = oldsave_ix - oldsave_start;
    }

    #[inline]
    #[allow(unused_variables)]
    fn trace_stack(&self, operation: &str) {
        #[cfg(feature = "std")]
        if self.options & OPTION_TRACE != 0 {
            println!("stack after {}: {:?}", operation, self.stack);
        }
    }
}

/// Run the program with trace printing for debugging.
pub fn run_trace(prog: &Prog, s: &str, pos: usize) -> Result<Option<Vec<usize>>> {
    run(prog, s, pos, OPTION_TRACE, &RegexOptions::default())
}

/// Run the program with default options.
pub fn run_default(prog: &Prog, s: &str, pos: usize) -> Result<Option<Vec<usize>>> {
    run(prog, s, pos, 0, &RegexOptions::default())
}

/// Run the program with options.
#[allow(clippy::cognitive_complexity)]
pub(crate) fn run(
    prog: &Prog,
    s: &str,
    pos: usize,
    option_flags: u32,
    options: &RegexOptions,
) -> Result<Option<Vec<usize>>> {
    let mut state = State::new(prog.n_saves, MAX_STACK, option_flags);
    let mut inner_slots: Vec<Option<NonMaxUsize>> = Vec::new();
    let look_matcher = LookMatcher::new();
    #[cfg(feature = "std")]
    if option_flags & OPTION_TRACE != 0 {
        println!("pos\tinstruction");
    }
    let mut backtrack_count = 0;
    let mut pc = 0;
    let mut ix = pos;
    loop {
        // break from this loop to fail, causes stack to pop
        'fail: loop {
            #[cfg(feature = "std")]
            if option_flags & OPTION_TRACE != 0 {
                println!("{}\t{} {:?}", ix, pc, prog.body[pc]);
            }
            match prog.body[pc] {
                Insn::End => {
                    // save of end position into slot 1 is now done
                    // with an explicit group; we might want to
                    // optimize that.
                    //state.saves[1] = ix;
                    #[cfg(feature = "std")]
                    if option_flags & OPTION_TRACE != 0 {
                        println!("saves: {:?}", state.saves);
                    }
                    if let Some(&slot1) = state.saves.get(1) {
                        // With some features like keep out (\K), the match start can be after
                        // the match end. Cap the start to <= end.
                        if state.get(0) > slot1 {
                            state.save(0, slot1);
                        }
                    }
                    return Ok(Some(state.saves));
                }
                Insn::Any => {
                    if ix < s.len() {
                        ix += codepoint_len_at(s, ix);
                    } else {
                        break 'fail;
                    }
                }
                Insn::AnyNoNL => {
                    if ix < s.len() && s.as_bytes()[ix] != b'\n' {
                        ix += codepoint_len_at(s, ix);
                    } else {
                        break 'fail;
                    }
                }
                Insn::Lit(ref val) => {
                    let ix_end = ix + val.len();
                    if !matches_literal(s, ix, ix_end, val) {
                        break 'fail;
                    }
                    ix = ix_end
                }
                Insn::Assertion(assertion) => {
                    if !match assertion {
                        Assertion::StartText => look_matcher.is_start(s.as_bytes(), ix),
                        Assertion::EndText => look_matcher.is_end(s.as_bytes(), ix),
                        Assertion::StartLine { crlf: false } => {
                            look_matcher.is_start_lf(s.as_bytes(), ix)
                        }
                        Assertion::StartLine { crlf: true } => {
                            look_matcher.is_start_crlf(s.as_bytes(), ix)
                        }
                        Assertion::EndLine { crlf: false } => {
                            look_matcher.is_end_lf(s.as_bytes(), ix)
                        }
                        Assertion::EndLine { crlf: true } => {
                            look_matcher.is_end_crlf(s.as_bytes(), ix)
                        }
                        Assertion::LeftWordBoundary => look_matcher
                            .is_word_start_unicode(s.as_bytes(), ix)
                            .unwrap(),
                        Assertion::RightWordBoundary => {
                            look_matcher.is_word_end_unicode(s.as_bytes(), ix).unwrap()
                        }
                        Assertion::LeftWordHalfBoundary => look_matcher
                            .is_word_start_half_unicode(s.as_bytes(), ix)
                            .unwrap(),
                        Assertion::RightWordHalfBoundary => look_matcher
                            .is_word_end_half_unicode(s.as_bytes(), ix)
                            .unwrap(),
                        Assertion::WordBoundary => {
                            look_matcher.is_word_unicode(s.as_bytes(), ix).unwrap()
                        }
                        Assertion::NotWordBoundary => look_matcher
                            .is_word_unicode_negate(s.as_bytes(), ix)
                            .unwrap(),
                    } {
                        break 'fail;
                    }
                }
                Insn::Split(x, y) => {
                    state.push(y, ix)?;
                    pc = x;
                    continue;
                }
                Insn::Jmp(target) => {
                    pc = target;
                    continue;
                }
                Insn::Save(slot) => state.save(slot, ix),
                Insn::Save0(slot) => state.save(slot, 0),
                Insn::Restore(slot) => ix = state.get(slot),
                Insn::RepeatGr {
                    lo,
                    hi,
                    next,
                    repeat,
                } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(next, ix)?;
                    }
                }
                Insn::RepeatNg {
                    lo,
                    hi,
                    next,
                    repeat,
                } => {
                    let repcount = state.get(repeat);
                    if repcount == hi {
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.push(pc + 1, ix)?;
                        pc = next;
                        continue;
                    }
                }
                Insn::RepeatEpsilonGr {
                    lo,
                    next,
                    repeat,
                    check,
                } => {
                    let repcount = state.get(repeat);
                    if repcount > 0 && state.get(check) == ix {
                        // zero-length match on repeat, then move to next instruction
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(next, ix)?;
                    }
                }
                Insn::RepeatEpsilonNg {
                    lo,
                    next,
                    repeat,
                    check,
                } => {
                    let repcount = state.get(repeat);
                    if repcount > 0 && state.get(check) == ix {
                        // zero-length match on repeat, then move to next instruction
                        pc = next;
                        continue;
                    }
                    state.save(repeat, repcount + 1);
                    if repcount >= lo {
                        state.save(check, ix);
                        state.push(pc + 1, ix)?;
                        pc = next;
                        continue;
                    }
                }
                Insn::GoBack(count) => {
                    for _ in 0..count {
                        if ix == 0 {
                            break 'fail;
                        }
                        ix = prev_codepoint_ix(s, ix);
                    }
                }
                Insn::FailNegativeLookAround => {
                    // Reaching this instruction means that the body of the
                    // look-around matched. Because it's a *negative* look-around,
                    // that means the look-around itself should fail (not match).
                    // But before, we need to discard all the states that have
                    // been pushed with the look-around, because we don't want to
                    // explore them.
                    loop {
                        let (popped_pc, _) = state.pop();
                        if popped_pc == pc + 1 {
                            // We've reached the state that would jump us to
                            // after the look-around (in case the look-around
                            // succeeded). That means we popped enough states.
                            break;
                        }
                    }
                    break 'fail;
                }
                Insn::Backref { slot, casei } => {
                    let lo = state.get(slot);
                    if lo == usize::MAX {
                        // Referenced group hasn't matched, so the backref doesn't match either
                        break 'fail;
                    }
                    let hi = state.get(slot + 1);
                    if hi == usize::MAX {
                        // Referenced group hasn't matched, so the backref doesn't match either
                        break 'fail;
                    }
                    let ref_text = &s[lo..hi];
                    let ix_end = ix + ref_text.len();
                    if casei {
                        if !matches_literal_casei(s, ix, ix_end, ref_text) {
                            break 'fail;
                        }
                    } else if !matches_literal(s, ix, ix_end, ref_text) {
                        break 'fail;
                    }
                    ix = ix_end;
                }
                Insn::BackrefExistsCondition(group) => {
                    let lo = state.get(group * 2);
                    if lo == usize::MAX {
                        // Referenced group hasn't matched, so the backref doesn't match either
                        break 'fail;
                    }
                }
                Insn::Fail => {
                    // Immediately fail and trigger backtracking
                    break 'fail;
                }
                #[cfg(feature = "variable-lookbehinds")]
                Insn::BackwardsDelegate(ReverseBackwardsDelegate {
                    ref dfa,
                    ref cache_pool,
                    pattern: _,
                    ref capture_group_extraction_inner,
                    capture_groups,
                }) => {
                    // Use regex-automata to search backwards from current position
                    let mut cache_guard = cache_pool.get();
                    let input = Input::new(s).anchored(Anchored::Yes).range(0..ix);

                    match dfa.try_search_rev(&mut cache_guard, &input) {
                        Ok(Some(match_result)) => {
                            // Update ix to the start position of the match
                            let match_start = match_result.offset();

                            if let Some(inner) = capture_group_extraction_inner {
                                if let Some(range) = capture_groups {
                                    // There are capture groups, need to search forward to populate them
                                    let forward_input =
                                        Input::new(s).span(match_start..ix).anchored(Anchored::Yes);
                                    inner_slots.resize((range.end() - range.start() + 1) * 2, None);

                                    if inner
                                        .search_slots(&forward_input, &mut inner_slots)
                                        .is_some()
                                    {
                                        // Store capture group positions
                                        store_capture_groups(&mut state, &inner_slots, range);
                                    } else {
                                        break 'fail;
                                    }
                                } else {
                                    // No groups, just update ix to the match start
                                    ix = match_start;
                                }
                            } else {
                                // No groups, just update ix to the match start
                                ix = match_start;
                            }
                        }
                        _ => break 'fail,
                    }
                }
                Insn::BeginAtomic => {
                    let count = state.backtrack_count();
                    state.stack_push(count);
                }
                Insn::EndAtomic => {
                    let count = state.stack_pop();
                    state.backtrack_cut(count);
                }
                Insn::Delegate(Delegate {
                    ref inner,
                    pattern: _,
                    capture_groups,
                }) => {
                    let input = Input::new(s).span(ix..s.len()).anchored(Anchored::Yes);
                    if let Some(range) = capture_groups {
                        // Has capture groups, need to extract them
                        inner_slots.resize((range.end() - range.start() + 1) * 2, None);
                        if inner.search_slots(&input, &mut inner_slots).is_some() {
                            store_capture_groups(&mut state, &inner_slots, range);
                            ix = inner_slots[1].unwrap().get();
                        } else {
                            break 'fail;
                        }
                    } else {
                        // No groups, so we can use faster methods
                        match inner.search_half(&input) {
                            Some(m) => ix = m.offset(),
                            _ => break 'fail,
                        }
                    }
                }
                Insn::ContinueFromPreviousMatchEnd { at_start } => {
                    if ix > pos || option_flags & OPTION_SKIPPED_EMPTY_MATCH != 0 {
                        // If \G is at the start of the pattern, we can fail early
                        // instead of checking at each position in the haystack
                        // because \G will never match at any other position
                        if at_start && state.stack.len() == 1 {
                            // The only item on the stack is from the Split instruction for non-anchored search
                            // We can safely return None immediately
                            return Ok(None);
                        }
                        break 'fail;
                    }
                }
            }
            pc += 1;
        }
        #[cfg(feature = "std")]
        if option_flags & OPTION_TRACE != 0 {
            println!("fail");
        }
        // "break 'fail" goes here
        if state.stack.is_empty() {
            return Ok(None);
        }

        backtrack_count += 1;
        if backtrack_count > options.backtrack_limit {
            return Err(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded));
        }

        let (newpc, newix) = state.pop();
        pc = newpc;
        ix = newix;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{quickcheck, Arbitrary, Gen};

    #[test]
    fn state_push_pop() {
        let mut state = State::new(1, MAX_STACK, 0);

        state.push(0, 0).unwrap();
        state.push(1, 1).unwrap();
        assert_eq!(state.pop(), (1, 1));
        assert_eq!(state.pop(), (0, 0));
        assert!(state.stack.is_empty());

        state.push(2, 2).unwrap();
        assert_eq!(state.pop(), (2, 2));
        assert!(state.stack.is_empty());
    }

    #[test]
    fn state_save_override() {
        let mut state = State::new(1, MAX_STACK, 0);
        state.save(0, 10);
        state.push(0, 0).unwrap();
        state.save(0, 20);
        assert_eq!(state.pop(), (0, 0));
        assert_eq!(state.get(0), 10);
    }

    #[test]
    fn state_save_override_twice() {
        let mut state = State::new(1, MAX_STACK, 0);
        state.save(0, 10);
        state.push(0, 0).unwrap();
        state.save(0, 20);
        state.push(1, 1).unwrap();
        state.save(0, 30);

        assert_eq!(state.get(0), 30);
        assert_eq!(state.pop(), (1, 1));
        assert_eq!(state.get(0), 20);
        assert_eq!(state.pop(), (0, 0));
        assert_eq!(state.get(0), 10);
    }

    #[test]
    fn state_explicit_stack() {
        let mut state = State::new(1, MAX_STACK, 0);
        state.stack_push(11);
        state.stack_push(12);

        state.push(100, 101).unwrap();
        state.stack_push(13);
        assert_eq!(state.stack_pop(), 13);
        state.stack_push(14);
        assert_eq!(state.pop(), (100, 101));

        // Note: 14 is not there because it was pushed as part of the backtrack branch
        assert_eq!(state.stack_pop(), 12);
        assert_eq!(state.stack_pop(), 11);
    }

    #[test]
    fn state_backtrack_cut_simple() {
        let mut state = State::new(2, MAX_STACK, 0);
        state.save(0, 1);
        state.save(1, 2);

        let count = state.backtrack_count();

        state.push(0, 0).unwrap();
        state.save(0, 3);
        assert_eq!(state.backtrack_count(), 1);

        state.backtrack_cut(count);
        assert_eq!(state.backtrack_count(), 0);
        assert_eq!(state.get(0), 3);
        assert_eq!(state.get(1), 2);
    }

    #[test]
    fn state_backtrack_cut_complex() {
        let mut state = State::new(2, MAX_STACK, 0);
        state.save(0, 1);
        state.save(1, 2);

        state.push(0, 0).unwrap();
        state.save(0, 3);

        let count = state.backtrack_count();

        state.push(1, 1).unwrap();
        state.save(0, 4);
        state.push(2, 2).unwrap();
        state.save(1, 5);
        assert_eq!(state.backtrack_count(), 3);

        state.backtrack_cut(count);
        assert_eq!(state.backtrack_count(), 1);
        assert_eq!(state.get(0), 4);
        assert_eq!(state.get(1), 5);

        state.pop();
        assert_eq!(state.backtrack_count(), 0);
        // Check that oldsave were set correctly
        assert_eq!(state.get(0), 1);
        assert_eq!(state.get(1), 2);
    }

    #[derive(Clone, Debug)]
    enum Operation {
        Push,
        Pop,
        Save(usize, usize),
    }

    impl Arbitrary for Operation {
        fn arbitrary(g: &mut Gen) -> Self {
            match g.choose(&[0, 1, 2]) {
                Some(0) => Operation::Push,
                Some(1) => Operation::Pop,
                _ => Operation::Save(
                    *g.choose(&[0usize, 1, 2, 3, 4]).unwrap(),
                    usize::arbitrary(g),
                ),
            }
        }
    }

    fn check_saves_for_operations(operations: Vec<Operation>) -> bool {
        let slots = operations
            .iter()
            .map(|o| match o {
                &Operation::Save(slot, _) => slot + 1,
                _ => 0,
            })
            .max()
            .unwrap_or(0);
        if slots == 0 {
            // No point checking if there's no save instructions
            return true;
        }

        // Stack with the complete VM state (including saves)
        let mut stack = Vec::new();
        let mut saves = vec![usize::MAX; slots];

        let mut state = State::new(slots, MAX_STACK, 0);

        let mut expected = Vec::new();
        let mut actual = Vec::new();

        for operation in operations {
            match operation {
                Operation::Push => {
                    // We're not checking pc and ix later, so don't bother
                    // putting in random values.
                    stack.push((0, 0, saves.clone()));
                    state.push(0, 0).unwrap();
                }
                Operation::Pop => {
                    // Note that because we generate the operations randomly
                    // there might be more pops than pushes. So ignore a pop
                    // if the stack was empty.
                    if let Some((_, _, previous_saves)) = stack.pop() {
                        saves = previous_saves;
                        state.pop();
                    }
                }
                Operation::Save(slot, value) => {
                    saves[slot] = value;
                    state.save(slot, value);
                }
            }

            // Remember state of saves for checking later
            expected.push(saves.clone());
            let mut actual_saves = vec![usize::MAX; slots];
            for (i, item) in actual_saves.iter_mut().enumerate().take(slots) {
                *item = state.get(i);
            }
            actual.push(actual_saves);
        }

        expected == actual
    }

    quickcheck! {
        fn state_save_quickcheck(operations: Vec<Operation>) -> bool {
            check_saves_for_operations(operations)
        }
    }
}
