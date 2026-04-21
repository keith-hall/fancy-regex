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

//! Compilation of regexes to VM.

use alloc::boxed::Box;
use alloc::format;
use alloc::string::{String, ToString};
#[cfg(feature = "variable-lookbehinds")]
use alloc::sync::Arc;
#[cfg(feature = "variable-lookbehinds")]
use alloc::vec;
use alloc::vec::Vec;
use regex_automata::meta::Regex as RaRegex;
use regex_automata::meta::{Builder as RaBuilder, Config as RaConfig};
#[cfg(feature = "variable-lookbehinds")]
use regex_automata::util::pool::Pool;
#[cfg(all(test, feature = "std"))]
use std::{collections::BTreeMap, sync::RwLock};

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

use crate::analyze::Info;
#[cfg(feature = "variable-lookbehinds")]
use crate::vm::{CachePoolFn, ReverseBackwardsDelegate};
use crate::vm::{CaptureGroupRange, Delegate, Insn, Prog, Seek};
use crate::LookAround::*;
use crate::{
    write_quantifier, Absent, Assertion, BacktrackingControlVerb, CompileError, Error, Expr,
    LookAround, RegexOptions, Result,
};

/// Maximum recursion depth for subroutine calls (matches Oniguruma's limit)
const MAX_SUBROUTINE_RECURSION_DEPTH: usize = 19;

// I'm thinking it probably doesn't make a lot of sense having this split
// out from Compiler.
struct VMBuilder {
    prog: Vec<Insn>,
    n_saves: usize,
}

impl VMBuilder {
    fn new(max_group: usize) -> VMBuilder {
        VMBuilder {
            prog: Vec::new(),
            n_saves: max_group * 2,
        }
    }

    fn build(self) -> Prog {
        Prog::new(self.prog, self.n_saves)
    }

    fn newsave(&mut self) -> usize {
        let result = self.n_saves;
        self.n_saves += 1;
        result
    }

    fn pc(&self) -> usize {
        self.prog.len()
    }

    // would "emit" be a better name?
    fn add(&mut self, insn: Insn) {
        self.prog.push(insn);
    }

    fn set_jmp_target(&mut self, jmp_pc: usize, target: usize) {
        match self.prog[jmp_pc] {
            Insn::Jmp(ref mut next) => *next = target,
            _ => panic!("mutating instruction other than Jmp"),
        }
    }

    fn set_split_target(&mut self, split_pc: usize, target: usize, second: bool) {
        match self.prog[split_pc] {
            Insn::Split(_, ref mut y) if second => *y = target,
            Insn::Split(ref mut x, _) => *x = target,
            _ => panic!("mutating instruction other than Split"),
        }
    }

    fn set_repeat_target(&mut self, repeat_pc: usize, target: usize) {
        match self.prog[repeat_pc] {
            Insn::RepeatGr { ref mut next, .. }
            | Insn::RepeatNg { ref mut next, .. }
            | Insn::RepeatEpsilonGr { ref mut next, .. }
            | Insn::RepeatEpsilonNg { ref mut next, .. } => *next = target,
            _ => panic!("mutating instruction other than Repeat"),
        }
    }
}

struct Compiler<'a> {
    b: VMBuilder,
    options: RegexOptions,
    inside_alternation: bool,
    /// Map from group number to its Info node for subroutine expansion
    group_info_map: Map<usize, &'a Info<'a>>,
    /// Stack tracking currently expanding subroutine calls to detect recursion depth
    subroutine_recursion_stack: Vec<usize>,
    /// Root Info node for handling group 0 subroutine calls
    root_info: Option<&'a Info<'a>>,
}

impl<'a> Compiler<'a> {
    fn new(max_group: usize) -> Compiler<'a> {
        Compiler {
            b: VMBuilder::new(max_group),
            options: Default::default(),
            inside_alternation: false,
            group_info_map: Map::new(),
            subroutine_recursion_stack: Vec::new(),
            root_info: None,
        }
    }

    fn visit(&mut self, info: &Info<'_>, hard: bool) -> Result<()> {
        if !hard && !info.hard {
            // easy case, delegate entire subexpr
            return self.compile_delegate(info);
        }
        match *info.expr {
            Expr::Empty => (),
            Expr::Literal { ref val, casei } => {
                if !casei {
                    self.b.add(Insn::Lit(val.clone()));
                } else {
                    self.compile_delegate(info)?;
                }
            }
            Expr::Any { newline: true, .. } => {
                self.b.add(Insn::Any);
            }
            Expr::Any {
                newline: false,
                crlf: true,
            } => {
                self.b.add(Insn::AnyNoCRLF);
            }
            Expr::Any {
                newline: false,
                crlf: false,
            } => {
                self.b.add(Insn::AnyNoNL);
            }
            Expr::GeneralNewline { unicode } => {
                self.compile_general_newline(unicode)?;
            }
            Expr::Concat(_) => {
                self.compile_concat(info, hard)?;
            }
            Expr::Alt(_) => {
                let count = info.children.len();
                let inside_alternation = self.inside_alternation;
                self.inside_alternation = true;
                self.compile_alt(count, |compiler, i| compiler.visit(&info.children[i], hard))?;
                self.inside_alternation = inside_alternation;
            }
            Expr::Group(_) => {
                let group = info.start_group();
                self.b.add(Insn::SaveCaptureGroupStart(group));
                self.visit(&info.children[0], hard)?;
                self.b.add(Insn::Save(group * 2 + 1));
            }
            Expr::Repeat { lo, hi, greedy, .. } => {
                self.compile_repeat(info, lo, hi, greedy, hard)?;
            }
            Expr::LookAround(_, la) => {
                self.compile_lookaround(info, la)?;
            }
            Expr::Backref { group, casei } => {
                self.b.add(Insn::Backref {
                    slot: group * 2,
                    casei,
                });
            }
            Expr::BackrefExistsCondition {
                group,
                relative_recursion_level: None,
            } => {
                self.b.add(Insn::BackrefExistsCondition(group));
            }
            Expr::BackrefExistsCondition {
                relative_recursion_level: Some(_),
                ..
            } => {
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported(
                        "Backref exists condition with relative recursion level".to_string(),
                    ),
                )));
            }
            Expr::BacktrackingControlVerb(BacktrackingControlVerb::Fail) => {
                self.b.add(Insn::Fail);
            }
            Expr::BacktrackingControlVerb(_) => {
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported(
                        "Backtracking control verbs other than 'fail'".to_string(),
                    ),
                )));
            }
            Expr::AtomicGroup(_) => {
                // TODO optimization: atomic insns are not needed if the
                // child doesn't do any backtracking.
                self.b.add(Insn::BeginAtomic);
                self.visit(&info.children[0], false)?;
                self.b.add(Insn::EndAtomic);
            }
            Expr::Delegate { .. } => {
                // TODO: might want to have more specialized impls
                self.compile_delegate(info)?;
            }
            Expr::Assertion(assertion) => {
                self.b.add(Insn::Assertion(assertion));
            }
            Expr::KeepOut => {
                self.b.add(Insn::Save(0));
            }
            Expr::ContinueFromPreviousMatchEnd => {
                self.b.add(Insn::ContinueFromPreviousMatchEnd {
                    at_start: info.start_group() == 1
                        && info.min_pos_in_group == 0
                        && !self.inside_alternation,
                });
            }
            Expr::Conditional { .. } => {
                self.compile_conditional(|compiler, i| compiler.visit(&info.children[i], hard))?;
            }
            Expr::SubroutineCall(target_group) => {
                // Check if we're already expanding this specific group (direct/indirect recursion)
                let recursion_count = self
                    .subroutine_recursion_stack
                    .iter()
                    .filter(|&&g| g == target_group)
                    .count();
                if recursion_count >= MAX_SUBROUTINE_RECURSION_DEPTH {
                    // Hit recursion limit - don't expand further, effectively making this match fail
                    // This matches Oniguruma's behavior of limiting recursion depth
                    self.b.add(Insn::Fail);
                    return Ok(());
                }

                // Handle group 0 (whole pattern) specially
                let target_info = if target_group == 0 {
                    self.root_info
                } else {
                    self.group_info_map.get(&target_group).map(|v| &**v)
                };

                if let Some(target_info) = target_info {
                    // Track that we're expanding this subroutine
                    self.subroutine_recursion_stack.push(target_group);

                    // For group 0, visit the entire root info
                    // For other groups, visit the child of the Group expression
                    if target_group == 0 {
                        self.visit(target_info, hard)?;
                    } else {
                        // Groups should always have at least one child (the group content)
                        // If empty, this is an error in the analysis phase
                        if target_info.children.is_empty() {
                            return Err(Error::CompileError(Box::new(
                                CompileError::UnexpectedGeneralError(format!(
                                    "Subroutine call to empty group {}",
                                    target_group
                                )),
                            )));
                        }
                        self.b.add(Insn::SaveCaptureGroupStart(target_group));
                        self.visit(&target_info.children[0], hard)?;
                        self.b.add(Insn::Save(target_group * 2 + 1));
                    }

                    // Pop the recursion stack
                    self.subroutine_recursion_stack.pop();
                } else {
                    // The target group doesn't exist (invalid group reference)
                    // This should have been caught by analysis, but be defensive
                    return Err(Error::CompileError(Box::new(
                        CompileError::SubroutineCallTargetNotFound(
                            format!(
                                "Invalid subroutine call to non-existent group {}",
                                target_group
                            ),
                            0,
                        ),
                    )));
                }
            }
            Expr::Absent(Absent::Repeater(_)) => {
                let child_info = &info.children[0];
                if child_info.hard {
                    return Err(Error::CompileError(Box::new(
                        CompileError::FeatureNotYetSupported(
                            "Absent repeater containing hard patterns".to_string(),
                        ),
                    )));
                }
                // Compile the child expression as a delegate
                let delegate = DelegateBuilder::new()
                    .push(child_info)
                    .build_delegate(&self.options)?;

                // Add the Absent instruction
                self.b.add(Insn::AbsentRepeater(delegate));
            }
            Expr::BackrefWithRelativeRecursionLevel { .. } => unreachable!(),
            Expr::Absent(ref absent) => {
                use crate::Absent::*;
                let error_msg = match absent {
                    Repeater(_) => "Absent repeater",
                    Expression { .. } => "Absent expression",
                    Stopper(_) => "Absent stopper",
                    Clear => "Range clear",
                };
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported(error_msg.to_string()),
                )));
            }
            Expr::AstNode { .. } => unreachable!("Should have been rejected during analysis"),
        }
        Ok(())
    }

    fn compile_alt<F>(&mut self, count: usize, mut handle_alternative: F) -> Result<()>
    where
        F: FnMut(&mut Compiler, usize) -> Result<()>,
    {
        let mut jmps = Vec::new();
        let mut last_pc = usize::MAX;
        for i in 0..count {
            let has_next = i != count - 1;
            let pc = self.b.pc();
            if has_next {
                self.b.add(Insn::Split(pc + 1, usize::MAX));
            }
            if last_pc != usize::MAX {
                self.b.set_split_target(last_pc, pc, true);
            }
            last_pc = pc;

            handle_alternative(self, i)?;

            if has_next {
                // All except the last branch need to jump over instructions of
                // other branches. The last branch can just continue to the next
                // instruction.
                let pc = self.b.pc();
                jmps.push(pc);
                self.b.add(Insn::Jmp(0));
            }
        }
        let next_pc = self.b.pc();
        for jmp_pc in jmps {
            self.b.set_jmp_target(jmp_pc, next_pc);
        }
        Ok(())
    }

    fn compile_conditional<F>(&mut self, mut handle_child: F) -> Result<()>
    where
        F: FnMut(&mut Compiler, usize) -> Result<()>,
    {
        // here we use atomic group functionality to be able to remove the program counter
        // relating to the split instruction's second position if the conditional succeeds
        // This is to ensure that if the condition succeeds, but the "true" branch from the
        // conditional fails, that it wouldn't jump to the "false" branch.
        self.b.add(Insn::BeginAtomic);

        let split_pc = self.b.pc();
        // add the split instruction - we will update it's second pc later
        self.b.add(Insn::Split(split_pc + 1, usize::MAX));

        // add the conditional expression
        handle_child(self, 0)?;

        // mark it as successful to remove the state we added as a split earlier
        self.b.add(Insn::EndAtomic);

        // add the truth branch
        handle_child(self, 1)?;
        // add an instruction to jump over the false branch - we will update the jump target later
        let jump_over_false_pc = self.b.pc();
        self.b.add(Insn::Jmp(0));

        // add the false branch, update the split target
        self.b.set_split_target(split_pc, self.b.pc(), true);
        handle_child(self, 2)?;

        // update the jump target for jumping over the false branch
        self.b.set_jmp_target(jump_over_false_pc, self.b.pc());

        Ok(())
    }

    fn compile_concat(&mut self, info: &Info<'_>, hard: bool) -> Result<()> {
        // First: determine a prefix which is constant size and not hard.
        let prefix_end = info
            .children
            .iter()
            .take_while(|c| c.const_size && !c.hard)
            .count();

        // If incoming difficulty is not hard, the suffix after the last
        // hard child can be done with NFA.
        let suffix_len = if !hard {
            info.children[prefix_end..]
                .iter()
                .rev()
                .take_while(|c| !c.hard)
                .count()
        } else {
            // Even for hard, we can delegate a const-sized suffix
            info.children[prefix_end..]
                .iter()
                .rev()
                .take_while(|c| c.const_size && !c.hard)
                .count()
        };
        let suffix_begin = info.children.len() - suffix_len;

        self.compile_delegates(&info.children[..prefix_end])?;

        for child in info.children[prefix_end..suffix_begin].iter() {
            self.visit(child, true)?;
        }

        self.compile_delegates(&info.children[suffix_begin..])
    }

    fn compile_repeat(
        &mut self,
        info: &Info<'_>,
        lo: usize,
        hi: usize,
        greedy: bool,
        hard: bool,
    ) -> Result<()> {
        let child = &info.children[0];
        if lo == 0 && hi == 0 {
            // zero repetition, matches empty string without executing child
            // This can happen with patterns like (abc){0}, which should match but never
            // execute the child expression or its capture groups
            return Ok(());
        }
        if lo == 0 && hi == 1 {
            // e?
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            // TODO: do we want to do an epsilon check here? If we do
            // it here and in Alt, we might be able to make a good
            // bound on stack depth
            self.visit(child, hard)?;
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
            return Ok(());
        }
        let hard = hard | info.hard;
        if hi == usize::MAX && child.min_size == 0 {
            // Use RepeatEpsilon instructions to prevent empty repeat
            let repeat = self.b.newsave();
            let check = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatEpsilonGr {
                    lo,
                    next: usize::MAX,
                    repeat,
                    check,
                });
            } else {
                self.b.add(Insn::RepeatEpsilonNg {
                    lo,
                    next: usize::MAX,
                    repeat,
                    check,
                });
            }
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        } else if lo == 0 && hi == usize::MAX {
            // e*
            let pc = self.b.pc();
            self.b.add(Insn::Split(pc + 1, pc + 1));
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_split_target(pc, next_pc, greedy);
        } else if lo == 1 && hi == usize::MAX {
            // e+
            let pc = self.b.pc();
            self.visit(child, hard)?;
            let next = self.b.pc() + 1;
            let (x, y) = if greedy { (pc, next) } else { (next, pc) };
            self.b.add(Insn::Split(x, y));
        } else {
            let repeat = self.b.newsave();
            self.b.add(Insn::Save0(repeat));
            let pc = self.b.pc();
            if greedy {
                self.b.add(Insn::RepeatGr {
                    lo,
                    hi,
                    next: usize::MAX,
                    repeat,
                });
            } else {
                self.b.add(Insn::RepeatNg {
                    lo,
                    hi,
                    next: usize::MAX,
                    repeat,
                });
            }
            self.visit(child, hard)?;
            self.b.add(Insn::Jmp(pc));
            let next_pc = self.b.pc();
            self.b.set_repeat_target(pc, next_pc);
        }
        Ok(())
    }

    fn compile_lookaround(&mut self, info: &Info<'_>, la: LookAround) -> Result<()> {
        let inner = &info.children[0];
        match la {
            LookBehind => {
                if let &Info {
                    const_size: false,
                    expr: &Expr::Alt(_),
                    ..
                } = inner
                {
                    // Make const size by transforming `(?<=a|bb)` to `(?<=a)|(?<=bb)`
                    let alternatives = &inner.children;
                    self.compile_alt(alternatives.len(), |compiler, i| {
                        let alternative = &alternatives[i];
                        compiler.compile_positive_lookaround(alternative, la)
                    })
                } else {
                    self.compile_positive_lookaround(inner, la)
                }
            }
            LookBehindNeg => {
                if let &Info {
                    const_size: false,
                    expr: &Expr::Alt(_),
                    ..
                } = inner
                {
                    // Make const size by transforming `(?<!a|bb)` to `(?<!a)(?<!bb)`
                    let alternatives = &inner.children;
                    for alternative in alternatives {
                        self.compile_negative_lookaround(alternative, la)?;
                    }
                    Ok(())
                } else {
                    self.compile_negative_lookaround(inner, la)
                }
            }
            LookAhead => self.compile_positive_lookaround(inner, la),
            LookAheadNeg => self.compile_negative_lookaround(inner, la),
        }
    }

    fn compile_positive_lookaround(&mut self, inner: &Info<'_>, la: LookAround) -> Result<()> {
        let save = self.b.newsave();
        self.b.add(Insn::Save(save));
        self.compile_lookaround_inner(inner, la)?;
        self.b.add(Insn::Restore(save));
        Ok(())
    }

    fn compile_negative_lookaround(&mut self, inner: &Info<'_>, la: LookAround) -> Result<()> {
        let pc = self.b.pc();
        self.b.add(Insn::Split(pc + 1, usize::MAX));
        self.compile_lookaround_inner(inner, la)?;
        self.b.add(Insn::FailNegativeLookAround);
        let next_pc = self.b.pc();
        self.b.set_split_target(pc, next_pc, true);
        Ok(())
    }

    fn compile_lookaround_inner(&mut self, inner: &Info<'_>, la: LookAround) -> Result<()> {
        if la == LookBehind || la == LookBehindNeg {
            if inner.const_size {
                self.b.add(Insn::GoBack(inner.min_size));
                self.visit(inner, false)
            } else if !inner.hard {
                #[cfg(feature = "variable-lookbehinds")]
                {
                    let mut delegate_builder = DelegateBuilder::new();
                    delegate_builder.push(inner);
                    self.compile_variable_lookbehind(delegate_builder)
                }
                #[cfg(not(feature = "variable-lookbehinds"))]
                {
                    Err(Error::CompileError(Box::new(
                        CompileError::VariableLookBehindRequiresFeature,
                    )))
                }
            } else {
                // If the variable lookbehind is a Concat expression where all children
                // are either easy or are guaranteed to consume 0 characters, then we can
                // compile it as variable lookbehind without additional goback instructions.
                if let Expr::Concat(_) = inner.expr {
                    let can_compile = inner
                        .children
                        .iter()
                        .all(|child| !child.hard || child.const_size);

                    if can_compile {
                        #[cfg(feature = "variable-lookbehinds")]
                        {
                            let mut delegate_nodes = vec![];
                            let mut go_back: usize = 0;
                            for child in inner.children.iter().rev() {
                                if child.hard {
                                    self.compile_variable_lookbehind_from_concat_nodes(
                                        &delegate_nodes,
                                    )?;
                                    delegate_nodes.clear();

                                    go_back += child.min_size;
                                    if go_back > 0 {
                                        self.b.add(Insn::GoBack(go_back));
                                    }
                                    self.visit(child, false)?;
                                    go_back = child.min_size;
                                } else {
                                    if go_back > 0 {
                                        self.b.add(Insn::GoBack(go_back));
                                        go_back = 0;
                                    }
                                    delegate_nodes.push(child);
                                }
                            }
                            self.compile_variable_lookbehind_from_concat_nodes(&delegate_nodes)?;
                            Ok(())
                        }
                        #[cfg(not(feature = "variable-lookbehinds"))]
                        {
                            Err(Error::CompileError(Box::new(
                                CompileError::VariableLookBehindRequiresFeature,
                            )))
                        }
                    } else {
                        Err(Error::CompileError(Box::new(
                            CompileError::FeatureNotYetSupported(
                                "Variable length lookbehinds with fancy features".to_string(),
                            ),
                        )))
                    }
                } else {
                    // variable sized lookbehinds with fancy features are currently unsupported
                    Err(Error::CompileError(Box::new(
                        CompileError::FeatureNotYetSupported(
                            "Variable length lookbehinds with fancy features".to_string(),
                        ),
                    )))
                }
            }
        } else {
            self.visit(inner, false)
        }
    }

    #[cfg(feature = "variable-lookbehinds")]
    fn compile_variable_lookbehind_from_concat_nodes(
        &mut self,
        infos: &Vec<&Info<'_>>,
    ) -> Result<()> {
        if infos.is_empty() {
            Ok(())
        } else {
            let mut delegate_builder = DelegateBuilder::new();
            for info in infos.iter().rev() {
                delegate_builder.push(info);
            }
            self.compile_variable_lookbehind(delegate_builder)
        }
    }

    #[cfg(feature = "variable-lookbehinds")]
    fn compile_variable_lookbehind(&mut self, delegate_builder: DelegateBuilder) -> Result<()> {
        let pattern = &delegate_builder.re;
        let capture_groups = delegate_builder
            .capture_groups
            .expect("Expected at least one expression");

        // Use reverse matching for variable-sized lookbehinds without fancy features
        use regex_automata::hybrid::dfa;
        use regex_automata::nfa::thompson;
        // Build a reverse DFA for the pattern
        let dfa = match dfa::DFA::builder()
            .configure(dfa::Config::new().unicode_word_boundary(true))
            .thompson(thompson::Config::new().reverse(true))
            .build(pattern)
        {
            Ok(dfa) => Arc::new(dfa),
            Err(e) => {
                return Err(Error::CompileError(Box::new(CompileError::DfaBuildError(
                    e.to_string(),
                ))))
            }
        };

        let create: CachePoolFn = alloc::boxed::Box::new({
            let dfa = Arc::clone(&dfa);
            move || dfa.create_cache()
        });
        let cache_pool = Pool::new(create);

        // Build the forward regex for capture group extraction
        let forward_regex = if capture_groups.start() != capture_groups.end() {
            Some(compile_inner(pattern, &self.options)?)
        } else {
            None
        };

        self.b
            .add(Insn::BackwardsDelegate(ReverseBackwardsDelegate {
                dfa,
                cache_pool,
                pattern: pattern.to_string(),
                capture_group_extraction_inner: forward_regex,
                capture_groups: capture_groups.to_option_if_non_empty(),
            }));
        Ok(())
    }

    fn compile_delegates(&mut self, infos: &[Info<'_>]) -> Result<()> {
        if infos.is_empty() {
            return Ok(());
        }
        // TODO: might want to do something similar for case insensitive literals
        // (have is_literal return an additional bool for casei)
        if infos.iter().all(|e| e.is_literal()) {
            let mut val = String::new();
            for info in infos {
                info.push_literal(&mut val);
            }
            self.b.add(Insn::Lit(val));
            return Ok(());
        }

        let mut delegate_builder = DelegateBuilder::new();
        for info in infos {
            delegate_builder.push(info);
        }
        let delegate = delegate_builder.build(&self.options)?;

        self.b.add(delegate);
        Ok(())
    }

    fn compile_delegate(&mut self, info: &Info) -> Result<()> {
        let insn = if info.is_literal() {
            let mut val = String::new();
            info.push_literal(&mut val);
            Insn::Lit(val)
        } else {
            DelegateBuilder::new().push(info).build(&self.options)?
        };
        self.b.add(insn);
        Ok(())
    }

    fn compile_general_newline(&mut self, unicode: bool) -> Result<()> {
        // Compile \R as: try \r\n first, then try single newline chars
        // The entire \R is atomic - once it matches, we don't backtrack
        // This prevents \r\n from backtracking to \r

        self.b.add(Insn::BeginAtomic);

        // Split: try \r\n first, then single chars
        let split_pc = self.b.pc();
        self.b.add(Insn::Split(split_pc + 1, usize::MAX)); // Will fix second target later

        // First alternative: \r\n
        self.b.add(Insn::Lit("\r\n".to_string()));

        // Jump over other alternatives
        let jmp_pc = self.b.pc();
        self.b.add(Insn::Jmp(usize::MAX)); // Will fix target later

        // Second alternative: single newline characters
        let single_newline_char_pc = self.b.pc();
        self.b
            .set_split_target(split_pc, single_newline_char_pc, true);

        // Compile a delegate for matching single newline characters
        let pattern = if unicode {
            // Unicode mode: \n, \v, \f, \r, U+0085, U+2028, U+2029
            "[\n\x0B\x0C\r\u{0085}\u{2028}\u{2029}]"
        } else {
            // Non-Unicode mode: \n, \v, \f, \r
            "[\n\x0B\x0C\r]"
        };

        let compiled = compile_inner(pattern, &self.options)?;
        self.b.add(Insn::Delegate(Delegate {
            inner: compiled,
            pattern: pattern.to_string(),
            capture_groups: None,
        }));

        // Fix the jump target
        let end_atomic_pc = self.b.pc();
        self.b.add(Insn::EndAtomic);

        self.b.set_jmp_target(jmp_pc, end_atomic_pc);

        Ok(())
    }
}

// Unlike Regex in `regex`, `regex-automata` does not store the pattern string,
// and we cannot retrieve the pattern string using `as_str`.
// Unfortunately we need to get the pattern string in our tests,
// so we just store it in a global map.
#[cfg(all(test, feature = "std"))]
static PATTERN_MAPPING: RwLock<BTreeMap<String, String>> = RwLock::new(BTreeMap::new());

pub(crate) fn compile_inner(inner_re: &str, options: &RegexOptions) -> Result<RaRegex> {
    let builder = options_to_rabuilder(options);

    let re = builder
        .build(inner_re)
        .map_err(CompileError::InnerError)
        .map_err(|e| Error::CompileError(Box::new(e)))?;

    #[cfg(all(test, feature = "std"))]
    PATTERN_MAPPING
        .write()
        .unwrap()
        .insert(format!("{:?}", re), inner_re.to_owned());

    Ok(re)
}

pub(crate) fn options_to_rabuilder(options: &RegexOptions) -> RaBuilder {
    let mut config = RaConfig::new();
    if let Some(limit) = options.delegate_size_limit {
        config = config.nfa_size_limit(Some(limit));
    }
    if let Some(limit) = options.delegate_dfa_size_limit {
        config = config.dfa_size_limit(Some(limit));
    }

    let mut builder = RaBuilder::new();
    builder.configure(config);
    builder.syntax(options.syntaxc);
    builder
}

/// Recursively populate the group_info_map with all capture groups in the Info tree
fn populate_group_info_map<'a>(map: &mut Map<usize, &'a Info<'a>>, info: &'a Info<'a>) {
    match info.expr {
        Expr::Group(_) => {
            let group = info.start_group();
            map.insert(group, info);
            // Continue recursing into children
            for child in &info.children {
                populate_group_info_map(map, child);
            }
        }
        _ => {
            // Recurse into all children
            for child in &info.children {
                populate_group_info_map(map, child);
            }
        }
    }
}

/// Returns `true` if the expression tree contains a `StartText` or `EndText` assertion anywhere.
///
/// This is used to decide whether it is safe to inline a group body when approximating a backref:
/// text-anchor assertions in an inlined body would be evaluated at the wrong string position and
/// could cause false negatives (missing valid match positions).
fn expr_contains_positional_anchor(expr: &Expr) -> bool {
    use crate::Assertion;
    match expr {
        // StartText (^), EndText ($), StartLine ((?m)^), EndLine ((?m)$) are positional anchors
        // that depend on absolute position in the string.  When a group containing such anchors
        // is inlined for a backref approximation, the anchors would be evaluated at the wrong
        // position (the backref position rather than the original capture position), potentially
        // producing false negatives.
        Expr::Assertion(
            Assertion::StartText
            | Assertion::EndText
            | Assertion::EndTextIgnoreTrailingNewlines { .. }
            | Assertion::StartLine { .. }
            | Assertion::EndLine { .. },
        ) => true,
        _ => expr
            .children_iter()
            .any(|child| expr_contains_positional_anchor(child)),
    }
}

/// Emit a permissive size placeholder — `(?s:.)+` or `(?s:.){N,}` — into `buf`.
///
/// Used when a hard node cannot be represented in the seek pattern but has a known minimum
/// size.  The placeholder is an over-approximation that avoids false negatives: it admits any
/// run of characters of at least `min_size` length.
fn emit_min_size_placeholder(buf: &mut String, min_size: usize, precedence: u8) {
    if min_size == 0 {
        return; // zero width: safe to drop without emitting anything
    }
    if precedence > 2 {
        buf.push_str("(?:");
    }
    buf.push_str("(?s:.)");
    match min_size {
        1 => buf.push('+'),
        n => {
            buf.push('{');
            crate::push_usize(buf, n);
            buf.push_str(",}");
        }
    }
    if precedence > 2 {
        buf.push(')');
    }
}

/// Write the seek-pattern approximation for `info` into `buf`.
///
/// Easy (non-hard) subtrees are serialised verbatim via [`Expr::to_str`].
/// Hard nodes are handled as follows:
/// - `Backref` / `SubroutineCall`: inline the referenced group's body (up to
///   `MAX_SUBROUTINE_RECURSION_DEPTH`). For backrefs, positional anchors (`^`, `$`, `(?m:^)`,
///   `(?m:$)`) are dropped from the inlined body — they hold at the original capture position
///   but not at the backref position. If inlining is not possible (depth limit, unknown group),
///   a permissive `(?s:.){min_size,}` placeholder is emitted instead of silently dropping.
/// - `LookAround`, `KeepOut`, `ContinueFromPreviousMatchEnd`, `BacktrackingControlVerb`,
///   `BackrefExistsCondition`, `Absent`: dropped (emit nothing — safe over-approximation,
///   their `min_size` is 0).
/// - `AtomicGroup`: keep the contents, discard the atomicity wrapper.
/// - `Group`: keep the contents, discard the capture group wrapper.
/// - `Conditional`: emit the union of the true and false branches.
/// - `GeneralNewline`: replaced with an explicit alternation.
/// - `Assertion`: anchors and word-boundary assertions are emitted; half-boundaries are
///   dropped (over-approximation — `\b` would be too restrictive).
///
/// The `drop_positional_anchors` flag suppresses positional anchor assertions (`^`, `$`, `(?m:^)`,
/// `(?m:$)`).  It is set to `true` when inlining a group body for a backref so that anchors
/// from the group definition are not incorrectly applied at the backref position.
///
/// # Code sharing with [`Expr::to_str`]
///
/// The repeat-quantifier serialisation is shared via [`write_quantifier`].  The structural
/// skeleton of `Concat`, `Alt` and `Repeat` mirrors `to_str`; the recursion callback differs
/// (here it recurses through the analysed [`Info`] tree rather than the raw [`Expr`] tree).
fn build_seek_pattern<'a>(
    info: &Info<'a>,
    group_info_map: &Map<usize, &'a Info<'a>>,
    depth: usize,
    buf: &mut String,
    precedence: u8,
) {
    build_seek_pattern_impl(info, group_info_map, depth, buf, precedence, false);
}

fn build_seek_pattern_impl<'a>(
    info: &Info<'a>,
    group_info_map: &Map<usize, &'a Info<'a>>,
    depth: usize,
    buf: &mut String,
    precedence: u8,
    drop_positional_anchors: bool,
) {
    // Drop positional anchors at this node when requested (used when inlining for a backref).
    if drop_positional_anchors {
        if let Expr::Assertion(
            Assertion::StartText
            | Assertion::EndText
            | Assertion::EndTextIgnoreTrailingNewlines { .. }
            | Assertion::StartLine { .. }
            | Assertion::EndLine { .. },
        ) = info.expr
        {
            return;
        }
    }

    if !info.hard {
        // Easy subtree — use to_str directly when no positional-anchor dropping is needed,
        // or when the subtree contains no positional anchors (so dropping is a no-op).
        if !drop_positional_anchors || !expr_contains_positional_anchor(info.expr) {
            info.expr.to_str(buf, precedence);
            return;
        }
        // The easy subtree contains positional anchors that need to be stripped.
        // Fall through to the match below, which recurses via build_seek_pattern_impl.
    }

    match info.expr {
        Expr::Empty => {}
        Expr::Assertion(assertion) => {
            // Emit all assertion types, approximating half-word-boundaries with \b.
            // Note: easy positional assertions (StartText, EndText, StartLine, EndLine) are
            // handled by the to_str early return above (drop_positional_anchors=false) or by
            // the early drop at the top of this function (drop_positional_anchors=true).
            // Only hard assertions reach this arm.
            match assertion {
                // \Z matches at the end of the string, or before any trailing newlines.
                // Approximate with `\n*$` (non-CRLF) or `(?:\r?\n)*$` (CRLF mode).
                // The `$` anchors to end-of-text, providing a useful seek filter.
                Assertion::EndTextIgnoreTrailingNewlines { crlf: false } => buf.push_str(r"\n*$"),
                Assertion::EndTextIgnoreTrailingNewlines { crlf: true } => {
                    buf.push_str(r"(?:\r?\n)*$")
                }
                Assertion::WordBoundary => buf.push_str(r"\b"),
                Assertion::NotWordBoundary => buf.push_str(r"\B"),
                // Full word boundaries: \< and \> — overapproximate with \b.
                Assertion::LeftWordBoundary | Assertion::RightWordBoundary => buf.push_str(r"\b"),
                // Half-boundaries (\b{start-half}, \b{end-half}) can match in positions where
                // \b does not (e.g. before punctuation), so \b would be an under-approximation.
                // Drop them (emit nothing) which is a safe over-approximation.
                Assertion::LeftWordHalfBoundary | Assertion::RightWordHalfBoundary => {}
                // Easy positional anchors — handled by early return / early drop above.
                // They can only reach here when drop_positional_anchors=false and the subtree
                // is easy (handled by to_str) but the Group/Concat parent fell through to match.
                // Emit them faithfully.
                Assertion::StartText => buf.push('^'),
                Assertion::EndText => buf.push('$'),
                Assertion::StartLine { crlf: false } => buf.push_str("(?m:^)"),
                Assertion::StartLine { crlf: true } => buf.push_str("(?Rm:^)"),
                Assertion::EndLine { crlf: false } => buf.push_str("(?m:$)"),
                Assertion::EndLine { crlf: true } => buf.push_str("(?Rm:$)"),
            }
        }
        Expr::Concat(_) => {
            if precedence > 1 {
                buf.push_str("(?:");
            }
            for child in &info.children {
                build_seek_pattern_impl(
                    child,
                    group_info_map,
                    depth,
                    buf,
                    2,
                    drop_positional_anchors,
                );
            }
            if precedence > 1 {
                buf.push(')');
            }
        }
        Expr::Alt(_) => {
            if precedence > 0 {
                buf.push_str("(?:");
            }
            let mut first = true;
            for child in &info.children {
                if !first {
                    buf.push('|');
                }
                build_seek_pattern_impl(
                    child,
                    group_info_map,
                    depth,
                    buf,
                    1,
                    drop_positional_anchors,
                );
                first = false;
            }
            if precedence > 0 {
                buf.push(')');
            }
        }
        Expr::Group(_) => {
            // Drop the capture group wrapper; keep the contents.
            if !info.children.is_empty() {
                build_seek_pattern_impl(
                    &info.children[0],
                    group_info_map,
                    depth,
                    buf,
                    precedence,
                    drop_positional_anchors,
                );
            }
        }
        Expr::Repeat { lo, hi, greedy, .. } => {
            if precedence > 2 {
                buf.push_str("(?:");
            }
            if !info.children.is_empty() {
                build_seek_pattern_impl(
                    &info.children[0],
                    group_info_map,
                    depth,
                    buf,
                    3,
                    drop_positional_anchors,
                );
            }
            write_quantifier(buf, *lo, *hi, *greedy);
            if precedence > 2 {
                buf.push(')');
            }
        }
        Expr::Backref { group, casei } => {
            // Inline the body of the referenced capture group, wrapping with (?i:...) when
            // the backref is case-insensitive so the approximation remains correct.
            //
            // Positional anchors are dropped from the inlined body: a backref matches the
            // captured text, not a position, so anchors from the group definition do not hold
            // at the backref's position.  Dropping them (rather than the whole backref) gives
            // a tighter over-approximation.
            //
            // If inlining is not possible (depth limit, group not in map), emit a permissive
            // placeholder so that no match positions are incorrectly skipped.
            if depth < MAX_SUBROUTINE_RECURSION_DEPTH {
                if let Some(group_info) = group_info_map.get(group) {
                    if !group_info.children.is_empty() {
                        let child = &group_info.children[0];
                        if *casei {
                            let mut inner = String::new();
                            build_seek_pattern_impl(
                                child,
                                group_info_map,
                                depth + 1,
                                &mut inner,
                                // Precedence 0 (alternation) so content inside (?i:...) is unambiguous.
                                0,
                                true,
                            );
                            if !inner.is_empty() {
                                buf.push_str("(?i:");
                                buf.push_str(&inner);
                                buf.push(')');
                            }
                        } else {
                            build_seek_pattern_impl(
                                child,
                                group_info_map,
                                depth + 1,
                                buf,
                                precedence,
                                true,
                            );
                        }
                        return;
                    }
                    // Empty group (no children): min_size=0, nothing to emit.
                    return;
                }
            }
            // Could not inline (depth limit or group not found): emit a permissive placeholder
            // so we don't falsely skip positions where the backref's target could have matched.
            emit_min_size_placeholder(buf, info.min_size, precedence);
        }
        Expr::SubroutineCall(target_group) => {
            // Inline the body of the target group, honouring the recursion depth limit.
            if depth < MAX_SUBROUTINE_RECURSION_DEPTH {
                if let Some(group_info) = group_info_map.get(target_group) {
                    if !group_info.children.is_empty() {
                        build_seek_pattern_impl(
                            &group_info.children[0],
                            group_info_map,
                            depth + 1,
                            buf,
                            precedence,
                            drop_positional_anchors,
                        );
                        return;
                    }
                    return;
                }
            }
            emit_min_size_placeholder(buf, info.min_size, precedence);
        }
        // LookAround is zero-width — drop it.
        Expr::LookAround(_, _) => {}
        Expr::AtomicGroup(_) => {
            // Keep the contents; atomicity is invisible to the seek approximation.
            if !info.children.is_empty() {
                build_seek_pattern_impl(
                    &info.children[0],
                    group_info_map,
                    depth,
                    buf,
                    precedence,
                    drop_positional_anchors,
                );
            }
        }
        Expr::GeneralNewline { unicode } => {
            // Replace \R with an explicit alternation accepted by regex-automata.
            if *unicode {
                buf.push_str(r"(?:\r\n|[\n\x0B\x0C\r\x85\u{2028}\u{2029}])");
            } else {
                buf.push_str(r"(?:\r\n|[\n\x0B\x0C\r])");
            }
        }
        Expr::Conditional { .. } => {
            // Emit the union of (condition + true_branch) and (false_branch).
            //
            // Including the condition prefix in the true alternative ensures that consuming
            // conditions (e.g. `(?(a)b|c)` where `a` is matched and consumed) are
            // over-approximated correctly.  For zero-width conditions (e.g. backref-exists
            // checks) the condition emits nothing and the result degenerates to
            // `true_branch | false_branch`, preserving the original behaviour.
            let mut cond_pat = String::new();
            let mut true_pat = String::new();
            let mut false_pat = String::new();
            if info.children.len() >= 1 {
                build_seek_pattern_impl(
                    &info.children[0],
                    group_info_map,
                    depth,
                    &mut cond_pat,
                    2,
                    drop_positional_anchors,
                );
            }
            if info.children.len() >= 2 {
                build_seek_pattern_impl(
                    &info.children[1],
                    group_info_map,
                    depth,
                    &mut true_pat,
                    2,
                    drop_positional_anchors,
                );
            }
            if info.children.len() >= 3 {
                build_seek_pattern_impl(
                    &info.children[2],
                    group_info_map,
                    depth,
                    &mut false_pat,
                    1,
                    drop_positional_anchors,
                );
            }
            // Build the "condition then true-branch" alternative.
            let cond_true = if cond_pat.is_empty() {
                true_pat.clone()
            } else if true_pat.is_empty() {
                cond_pat.clone()
            } else {
                format!("(?:{}{})", cond_pat, true_pat)
            };
            match (cond_true.is_empty(), false_pat.is_empty()) {
                (true, true) => {}
                (true, false) => buf.push_str(&false_pat),
                (false, true) => buf.push_str(&cond_true),
                (false, false) => {
                    if precedence > 0 {
                        buf.push_str("(?:");
                    }
                    buf.push_str(&cond_true);
                    buf.push('|');
                    buf.push_str(&false_pat);
                    if precedence > 0 {
                        buf.push(')');
                    }
                }
            }
        }
        // Absent repeater `(?~expr)` matches any content not containing `expr`.
        // Approximate with `(?s:.*)` (any characters, including newline) since the repeater
        // can consume an arbitrary number of characters.
        Expr::Absent(Absent::Repeater(_)) => buf.push_str("(?s:.*)"),
        // Absent expression `(?~|absent|exp)` matches `exp` subject to the absent constraint.
        // Approximate by seeking only for `exp`, dropping the constraint — this is a safe
        // over-approximation since any position where `exp` matches (ignoring the constraint)
        // is a superset of positions where the full expression matches.
        Expr::Absent(crate::Absent::Expression { .. }) => {
            if info.children.len() >= 2 {
                build_seek_pattern_impl(
                    &info.children[1],
                    group_info_map,
                    depth,
                    buf,
                    precedence,
                    drop_positional_anchors,
                );
            }
        }
        // Zero-width / control nodes — drop them (all have min_size = 0).
        Expr::KeepOut
        | Expr::ContinueFromPreviousMatchEnd
        | Expr::BacktrackingControlVerb(_)
        | Expr::BackrefExistsCondition { .. }
        | Expr::Absent(_) => {}
        // Easy leaf nodes (Literal, Any, Delegate) are always handled by the `!info.hard`
        // early return above and never reach here.  Listed explicitly so that adding a new
        // Expr variant produces a compile error until the seek-pattern case is handled.
        Expr::Literal { .. } | Expr::Any { .. } | Expr::Delegate { .. } => {
            info.expr.to_str(buf, precedence)
        }
        // These variants cause a compile error during analysis and are therefore unreachable
        // after a successful `analyze()` call.
        Expr::BackrefWithRelativeRecursionLevel { .. } | Expr::AstNode(..) => {
            unreachable!("unexpected expr variant after analysis")
        }
    }
}
/// Returns `true` if `pattern` contains at least one character that provides useful filtering
/// (i.e. a literal character, character class, or anchor rather than just wildcards/quantifiers).
///
/// This is a conservative approximation: any pattern containing `[`, `\`, `^`, `$`, or an
/// alphanumeric character is considered useful.  A character class like `[*]` technically
/// matches only a single character which is no better than `.`, but it is uncommon enough to
/// matter in practice and will still be admitted by this check.  The primary goal is to reject
/// fully unconstrained patterns such as `.*`.
fn seek_pattern_is_useful(pattern: &str) -> bool {
    pattern
        .bytes()
        .any(|b| matches!(b, b'[' | b'\\' | b'^' | b'$' | b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9'))
}

/// Options for compiling analyzed expressions into a program.
#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    /// Whether the regex is anchored (starts matching at the beginning of the input).
    /// When `false`, a `SplitUnanchored` preamble is emitted to allow matching at any position.
    pub anchored: bool,
    /// Whether the regex contains subroutine calls, requiring group info to be pre-populated.
    pub contains_subroutines: bool,
    /// Whether to attempt to derive and use a Seek pre-filter for hard patterns.
    /// When `true` and a useful approximation can be built, the `SplitUnanchored` preamble is
    /// replaced with a `Seek` instruction that jumps directly to plausible match positions.
    pub seek: bool,
}

/// Compile the analyzed expressions into a program.
pub fn compile(info: &Info<'_>, options: CompileOptions) -> Result<Prog> {
    let mut c = Compiler::new(info.end_group());

    if options.contains_subroutines {
        // Store root info for group 0 subroutine calls
        c.root_info = Some(info);

        // Pre-populate the group_info_map to support forward references
        populate_group_info_map(&mut c.group_info_map, info);
    }

    if !options.anchored {
        // Attempt to build a Seek pre-filter when requested.
        let mut used_seek = false;
        if options.seek && info.hard {
            // Build the group_info_map if not already populated (needed for backref inlining).
            let mut local_group_info_map: Map<usize, &Info<'_>>;
            let group_info_map: &Map<usize, &Info<'_>> = if options.contains_subroutines {
                &c.group_info_map
            } else {
                local_group_info_map = Map::new();
                populate_group_info_map(&mut local_group_info_map, info);
                &local_group_info_map
            };

            let mut seek_pat = String::new();
            build_seek_pattern(info, group_info_map, 0, &mut seek_pat, 0);

            if seek_pattern_is_useful(&seek_pat) {
                match compile_inner(&seek_pat, &c.options) {
                    Ok(inner) => {
                        c.b.add(Insn::Seek(Seek {
                            inner,
                            pattern: seek_pat,
                        }));
                        used_seek = true;
                    }
                    // If compilation of the seek pattern fails for any reason, fall back to the
                    // standard SplitUnanchored preamble.
                    Err(_) => {}
                }
            }
        }

        if !used_seek {
            // add instructions as if \O*? was used at the start of the expression
            // so that we bump the haystack index by one when failing to match at the current position
            let current_pc = c.b.pc();
            // we are adding 3 instructions, so the current program counter plus 3 gives us the first real instruction
            c.b.add(Insn::SplitUnanchored(current_pc + 3, current_pc + 1));
            c.b.add(Insn::Any);
            c.b.add(Insn::Jmp(current_pc));
        }
    }
    if info.start_group() == 1 {
        // add implicit capture group 0 begin
        c.b.add(Insn::Save(0));
    }
    c.visit(info, false)?;
    if info.start_group() == 1 {
        // add implicit capture group 0 end
        c.b.add(Insn::Save(1));
    }
    c.b.add(Insn::End);
    Ok(c.b.build())
}

struct DelegateBuilder {
    re: String,
    min_size: usize,
    const_size: bool,
    capture_groups: Option<CaptureGroupRange>,
}

impl DelegateBuilder {
    fn new() -> Self {
        Self {
            re: String::new(),
            min_size: 0,
            const_size: true,
            capture_groups: None,
        }
    }

    fn push(&mut self, info: &Info<'_>) -> &mut DelegateBuilder {
        // TODO: might want to detect case of a group with no captures
        //  inside, so we can run find() instead of captures()

        self.min_size += info.min_size;
        self.const_size &= info.const_size;
        if self.capture_groups.is_none() {
            self.capture_groups = Some(info.capture_groups);
        } else {
            // Update the end_group to the latest
            self.capture_groups = self
                .capture_groups
                .map(|range| CaptureGroupRange(range.start(), info.end_group()));
        }

        // Add expression. The precedence argument has to be 1 here to
        // ensure correct grouping in these cases:
        //
        // If we have multiple expressions, we are building a concat.
        // Without grouping, we'd turn ["a", "b|c"] into "^ab|c". But we
        // want "^a(?:b|c)".
        //
        // Even with a single expression, because we add `^` at the
        // beginning, we need a group. Otherwise `["a|b"]` would be turned
        // into `"^a|b"` instead of `"^(?:a|b)"`.
        info.expr.to_str(&mut self.re, 1);
        self
    }

    fn build(&self, options: &RegexOptions) -> Result<Insn> {
        Ok(Insn::Delegate(self.build_delegate(options)?))
    }

    fn build_delegate(&self, options: &RegexOptions) -> Result<Delegate> {
        let capture_groups = self
            .capture_groups
            .expect("Expected at least one expression");

        let compiled = compile_inner(&self.re, options)?;

        Ok(Delegate {
            inner: compiled,
            pattern: self.re.clone(),
            capture_groups: capture_groups.to_option_if_non_empty(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyze::{analyze, AnalyzeContext};
    use crate::parse::ExprTree;
    use crate::vm::Insn::*;
    use alloc::vec;
    use bit_set::BitSet;
    use matches::assert_matches;

    #[cfg_attr(feature = "track_caller", track_caller)]
    fn assert_compile_error<T, F>(result: crate::Result<T>, check: F)
    where
        F: FnOnce(&CompileError) -> bool,
    {
        match result {
            Err(Error::CompileError(ref e)) if check(e) => {}
            other => panic!(
                "expected a matching CompileError, but got: {:?}",
                other.err()
            ),
        }
    }

    #[test]
    fn jumps_for_alternation() {
        let tree = ExprTree {
            expr: Expr::Alt(vec![
                Expr::Literal {
                    val: "a".into(),
                    casei: false,
                },
                Expr::Literal {
                    val: "b".into(),
                    casei: false,
                },
                Expr::Literal {
                    val: "c".into(),
                    casei: false,
                },
            ]),
            backrefs: BitSet::new(),
            named_groups: Default::default(),
            numeric_capture_group_references: false,
            contains_subroutines: false,
            self_recursive: false,
            total_groups: 0,
            out_of_range_backref: None,
            numbered_groups_ignored: false,
        };
        let info = analyze(&tree, AnalyzeContext::default()).unwrap();

        let mut c = Compiler::new(0);
        // Force "hard" so that compiler doesn't just delegate
        c.visit(&info, true).unwrap();
        c.b.add(Insn::End);

        let prog = c.b.prog;

        assert_eq!(prog.len(), 8, "prog: {:?}", prog);
        assert_matches!(prog[0], Split(1, 3));
        assert_matches!(prog[1], Lit(ref l) if l == "a");
        assert_matches!(prog[2], Jmp(7));
        assert_matches!(prog[3], Split(4, 6));
        assert_matches!(prog[4], Lit(ref l) if l == "b");
        assert_matches!(prog[5], Jmp(7));
        assert_matches!(prog[6], Lit(ref l) if l == "c");
        assert_matches!(prog[7], End);
    }

    #[cfg_attr(not(feature = "std"), ignore = "this test need std")]
    #[test]
    fn look_around_pattern_can_be_delegated() {
        let prog = compile_prog("(?=ab*)c");

        assert_eq!(prog.len(), 5, "prog: {:?}", prog);
        assert_matches!(prog[0], Save(0));
        assert_delegate_insn(&prog[1], "ab*", None);
        assert_matches!(prog[2], Restore(0));
        assert_matches!(prog[3], Lit(ref l) if l == "c");
        assert_matches!(prog[4], End);
    }

    #[cfg_attr(not(feature = "std"), ignore = "this test need std")]
    #[test]
    fn easy_concat_can_delegate_end() {
        let prog = compile_prog("(?!x)(?:a|ab)x*");

        assert_eq!(prog.len(), 5, "prog: {:?}", prog);
        assert_matches!(prog[0], Split(1, 3));
        assert_matches!(prog[1], Lit(ref l) if l == "x");
        assert_matches!(prog[2], FailNegativeLookAround);
        assert_delegate_insn(&prog[3], "(?:a|ab)x*", None);
        assert_matches!(prog[4], End);
    }

    #[cfg_attr(not(feature = "std"), ignore = "this test need std")]
    #[test]
    fn hard_concat_can_delegate_const_size_end() {
        let prog = compile_prog("(?:(?!x)(?:a|b)c)x*");

        assert_eq!(prog.len(), 6, "prog: {:?}", prog);
        assert_matches!(prog[0], Split(1, 3));
        assert_matches!(prog[1], Lit(ref l) if l == "x");
        assert_matches!(prog[2], FailNegativeLookAround);
        assert_delegate_insn(&prog[3], "(?:a|b)c", None);
        assert_delegate_insn(&prog[4], "x*", None);
        assert_matches!(prog[5], End);
    }

    #[cfg_attr(not(feature = "std"), ignore = "this test need std")]
    #[test]
    fn hard_concat_can_not_delegate_variable_end() {
        let prog = compile_prog("(?:(?!x)(?:a|ab))x*");

        assert_eq!(prog.len(), 9, "prog: {:?}", prog);
        assert_matches!(prog[0], Split(1, 3));
        assert_matches!(prog[1], Lit(ref l) if l == "x");
        assert_matches!(prog[2], FailNegativeLookAround);
        assert_matches!(prog[3], Split(4, 6));
        assert_matches!(prog[4], Lit(ref l) if l == "a");
        assert_matches!(prog[5], Jmp(7));
        assert_matches!(prog[6], Lit(ref l) if l == "ab");
        assert_delegate_insn(&prog[7], "x*", None);
        assert_matches!(prog[8], End);
    }

    #[test]
    fn conditional_expression_can_be_compiled() {
        let prog = compile_prog(r"(?(ab)c|d)");

        assert_eq!(prog.len(), 8, "prog: {:?}", prog);

        assert_matches!(prog[0], BeginAtomic);
        assert_matches!(prog[1], Split(2, 6));
        assert_matches!(prog[2], Lit(ref l) if l == "ab");
        assert_matches!(prog[3], EndAtomic);
        assert_matches!(prog[4], Lit(ref l) if l == "c");
        assert_matches!(prog[5], Jmp(7));
        assert_matches!(prog[6], Lit(ref l) if l == "d");
        assert_matches!(prog[7], End);
    }

    #[test]
    fn lazy_any_can_be_compiled_explicit_capture_group_zero() {
        let prog = compile_prog(r"\O*?((?!a))");

        assert_eq!(prog.len(), 9, "prog: {:?}", prog);

        assert_matches!(prog[0], Split(3, 1));
        assert_matches!(prog[1], Any);
        assert_matches!(prog[2], Jmp(0));
        assert_matches!(prog[3], SaveCaptureGroupStart(0));
        assert_matches!(prog[4], Split(5, 7));
        assert_matches!(prog[5], Lit(ref l) if l == "a");
        assert_matches!(prog[6], FailNegativeLookAround);
        assert_matches!(prog[7], Save(1));
        assert_matches!(prog[8], End);
    }

    #[test]
    fn backtracking_control_verb_fail_can_be_compiled() {
        let prog = compile_prog(r"(*FAIL)");

        assert_eq!(prog.len(), 2, "prog: {:?}", prog);

        assert_matches!(prog[0], Fail);
        assert_matches!(prog[1], End);
    }

    #[test]
    fn other_backtracking_control_verbs_error() {
        let tree = Expr::parse_tree(r"(*ACCEPT)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );

        let tree = Expr::parse_tree(r"(*COMMIT)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );

        let tree = Expr::parse_tree(r"(*SKIP)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );

        let tree = Expr::parse_tree(r"(*PRUNE)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );
    }

    #[test]
    fn backref_exists_condition_with_recursion_level_not_yet_supported() {
        for pattern in &[
            r"(a)(?(1+0)b|c)d",
            r"(?<n>a)(?(<n+0>)b|c)d",
            r"(?<n>a)(?('n+0')b|c)d",
        ] {
            let tree = Expr::parse_tree(pattern).unwrap();
            let info = analyze(
                &tree,
                AnalyzeContext {
                    explicit_capture_group_0: false,
                    ..Default::default()
                },
            )
            .unwrap();
            assert_compile_error(
                compile(
                    &info,
                    CompileOptions {
                        anchored: true,
                        contains_subroutines: tree.contains_subroutines,
                        ..CompileOptions::default()
                    },
                ),
                |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
            );
        }
    }

    #[test]
    #[cfg(not(feature = "variable-lookbehinds"))]
    fn variable_lookbehind_requires_feature() {
        // Without the feature flag, variable-length lookbehinds should error
        let tree = Expr::parse_tree(r"(?<=ab+)x").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::VariableLookBehindRequiresFeature),
        );

        let tree = Expr::parse_tree(r"(?<=\bab+)x").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::VariableLookBehindRequiresFeature),
        );
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_no_captures_easy() {
        let prog = compile_prog(r"(?<=ab+)x");

        assert_eq!(prog.len(), 5, "prog: {:?}", prog);

        assert_matches!(prog[0], Save(0));
        assert_matches!(&prog[1], BackwardsDelegate(ReverseBackwardsDelegate { pattern, dfa: _, cache_pool: _, capture_group_extraction_inner: None, capture_groups: None }) if pattern == "ab+");
        assert_matches!(prog[2], Restore(0));
        assert_matches!(prog[3], Lit(ref l) if l == "x");
        assert_matches!(prog[4], End);
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_no_captures_hard_const_size_zero_length() {
        let prog = compile_prog(r"(?<=\bab+)x");

        assert_eq!(prog.len(), 6, "prog: {:?}", prog);

        assert_matches!(prog[0], Save(0));
        assert_matches!(&prog[1], BackwardsDelegate(ReverseBackwardsDelegate { pattern, dfa: _, cache_pool: _, capture_group_extraction_inner: None, capture_groups: None }) if pattern == "ab+");
        assert_matches!(prog[2], Insn::Assertion(crate::Assertion::WordBoundary));
        assert_matches!(prog[3], Restore(0));
        assert_matches!(prog[4], Lit(ref l) if l == "x");
        assert_matches!(prog[5], End);
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_no_captures_hard_const_size_non_zero_length() {
        let prog = compile_prog(r"((.)b+(?<=\1\1b+)x)");

        assert_eq!(prog.len(), 16, "prog: {:?}", prog);

        assert_matches!(prog[0], SaveCaptureGroupStart(0));
        assert_matches!(prog[1], SaveCaptureGroupStart(1));
        assert_matches!(prog[2], AnyNoNL);
        assert_matches!(prog[3], Save(3));
        assert_matches!(prog[4], Lit(ref l) if l == "b");
        assert_matches!(prog[5], Split(4, 6));
        assert_matches!(prog[6], Save(4));
        assert_matches!(&prog[7], BackwardsDelegate(ReverseBackwardsDelegate { pattern, dfa: _, cache_pool: _, capture_group_extraction_inner: None, capture_groups: None }) if pattern == "b+");
        assert_matches!(prog[8], GoBack(1));
        assert_matches!(
            prog[9],
            Backref {
                slot: 2,
                casei: false
            }
        );
        assert_matches!(prog[10], GoBack(2));
        assert_matches!(
            prog[11],
            Backref {
                slot: 2,
                casei: false
            }
        );
        assert_matches!(prog[12], Restore(4));
        assert_matches!(prog[13], Lit(ref l) if l == "x");
        assert_matches!(prog[14], Save(1));
        assert_matches!(prog[15], End);
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_captures() {
        let prog = compile_prog(r"(?<=a(b+))x");

        assert_eq!(prog.len(), 5, "prog: {:?}", prog);

        assert_matches!(prog[0], Save(2));
        assert_matches!(&prog[1], BackwardsDelegate(ReverseBackwardsDelegate { pattern, dfa: _, cache_pool: _, capture_group_extraction_inner: ref inner, capture_groups: Some(CaptureGroupRange(0, 1)) }) if pattern == "a(b+)" && inner.is_some());
        assert_matches!(prog[2], Restore(2));
        assert_matches!(prog[3], Lit(ref l) if l == "x");
        assert_matches!(prog[4], End);
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_backref_captures() {
        // currently hard variable lookbehinds are unsupported.
        // the backref to a capture group inside the variable lookbehind makes the capture group hard
        let tree = Expr::parse_tree(r"(?<=a(b+))\1").unwrap();
        let info = analyze(&tree, AnalyzeContext::default()).unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );
    }

    #[test]
    fn absent_repeater_with_easy_inner_compiles() {
        let prog = compile_prog(r"((?~abc))");

        assert_eq!(prog.len(), 4, "prog: {:?}", prog);
        assert_matches!(prog[0], SaveCaptureGroupStart(0));
        assert_absent_repeater_insn(&prog[1], "abc", None);
        assert_matches!(prog[2], Save(1));
        assert_matches!(prog[3], End);
    }

    #[test]
    fn absent_operators_error() {
        // Test that absent expression returns feature not supported
        let tree = Expr::parse_tree(r"(?~|abc|\d*)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );

        // Test that absent stopper returns feature not supported
        let tree = Expr::parse_tree(r"(?~|abc)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );

        // Test that range clear returns feature not supported
        let tree = Expr::parse_tree(r"(?~|)").unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_compile_error(
            compile(
                &info,
                CompileOptions {
                    anchored: true,
                    contains_subroutines: tree.contains_subroutines,
                    ..CompileOptions::default()
                },
            ),
            |e| matches!(e, CompileError::FeatureNotYetSupported(_)),
        );
    }

    #[test]
    fn single_delegate_with_capture_groups_can_be_compiled() {
        let prog = compile_prog(r"(.(b)([^a]+))c");

        assert_eq!(prog.len(), 2, "prog: {:?}", prog);
        assert_delegate_insn(&prog[0], "(.(b)([^a]+))c", Some(CaptureGroupRange(0, 3)));
        assert_matches!(prog[1], End);
    }

    #[test]
    fn delegated_capture_groups_can_be_compiled() {
        let prog = compile_prog(r"(.(b)([^a]+)(?!c)(\w))");

        assert_eq!(prog.len(), 12, "prog: {:?}", prog);

        assert_matches!(prog[0], SaveCaptureGroupStart(0));
        assert_delegate_insn(&prog[1], ".(b)", Some(CaptureGroupRange(1, 2)));
        assert_matches!(prog[2], SaveCaptureGroupStart(2));
        assert_delegate_insn(&prog[3], "[^a]", None);
        assert_matches!(prog[4], Split(3, 5));
        assert_matches!(prog[5], Save(5));
        assert_matches!(prog[6], Split(7, 9));
        assert_matches!(prog[7], Lit(ref l) if l == "c");
        assert_matches!(prog[8], FailNegativeLookAround);
        assert_delegate_insn(&prog[9], r"(\w)", Some(CaptureGroupRange(3, 4)));
        assert_matches!(prog[10], Save(1));
        assert_matches!(prog[11], End);
    }

    #[test]
    fn subroutine_call_can_be_compiled() {
        let prog = compile_prog(r"((.)\g<1>)");

        assert_eq!(prog.len(), 7, "prog: {:?}", prog);

        assert_matches!(prog[0], SaveCaptureGroupStart(0));
        assert_delegate_insn(&prog[1], "(.)", Some(CaptureGroupRange(1, 2)));
        assert_matches!(prog[2], SaveCaptureGroupStart(1));
        assert_matches!(prog[3], AnyNoNL);
        assert_matches!(prog[4], Save(3));
        assert_matches!(prog[5], Save(1));
        assert_matches!(prog[6], End);
    }

    #[test]
    fn forward_reference_subroutine_call_can_be_compiled() {
        let prog = compile_prog(r"(\g<1>(.))");

        assert_eq!(prog.len(), 7, "prog: {:?}", prog);

        assert_matches!(prog[0], SaveCaptureGroupStart(0));
        assert_matches!(prog[1], SaveCaptureGroupStart(1));
        assert_matches!(prog[2], AnyNoNL);
        assert_matches!(prog[3], Save(3));
        assert_delegate_insn(&prog[4], "(.)", Some(CaptureGroupRange(1, 2)));
        assert_matches!(prog[5], Save(1));
        assert_matches!(prog[6], End);
    }

    fn compile_prog(re: &str) -> Vec<Insn> {
        let tree = Expr::parse_tree(re).unwrap();
        let info = analyze(
            &tree,
            AnalyzeContext {
                explicit_capture_group_0: true,
                ..Default::default()
            },
        )
        .unwrap();
        let prog = compile(
            &info,
            CompileOptions {
                anchored: true,
                contains_subroutines: tree.contains_subroutines,
                ..CompileOptions::default()
            },
        )
        .unwrap();
        prog.body
    }

    fn assert_delegate_insn(insn: &Insn, re: &str, captures: Option<CaptureGroupRange>) {
        match insn {
            Insn::Delegate(delegate) => assert_delegate(delegate, re, captures),
            _ => {
                panic!("Expected Insn::Delegate but was {:#?}", insn);
            }
        }
    }

    fn assert_absent_repeater_insn(insn: &Insn, re: &str, captures: Option<CaptureGroupRange>) {
        match insn {
            Insn::AbsentRepeater(delegate) => assert_delegate(delegate, re, captures),
            _ => {
                panic!("Expected Insn::AbsentRepeater but was {:#?}", insn);
            }
        }
    }

    #[cfg(feature = "std")]
    fn assert_delegate(
        delegate: &crate::vm::Delegate,
        re: &str,
        captures: Option<CaptureGroupRange>,
    ) {
        assert_eq!(
            PATTERN_MAPPING
                .read()
                .unwrap()
                .get(&alloc::format!("{:?}", delegate.inner))
                .unwrap(),
            re
        );
        assert_eq!(captures, delegate.capture_groups);
    }

    #[cfg(not(feature = "std"))]
    fn assert_delegate(_: &crate::vm::Delegate, _: &str, _: Option<CaptureGroupRange>) {}

    // --- seek pattern tests ---

    /// Build the seek pattern for a regex string and return it.
    fn get_seek_pattern(re: &str) -> String {
        let tree = Expr::parse_tree(re).unwrap();
        let info = analyze(&tree, AnalyzeContext::default()).unwrap();
        let mut group_info_map = Map::new();
        populate_group_info_map(&mut group_info_map, &info);
        let mut buf = String::new();
        build_seek_pattern(&info, &group_info_map, 0, &mut buf, 0);
        buf
    }

    #[test]
    fn seek_pattern_backref_no_anchor() {
        // Simple backref with no positional anchors: group body is inlined verbatim.
        // The `(?:...)` wrapping comes from Concat precedence handling when stripping groups.
        assert_eq!(get_seek_pattern(r"(abc)\1"), "(?:abc)(?:abc)");
    }

    #[test]
    fn seek_pattern_backref_with_start_anchor() {
        // Backref to a group whose body starts with `^`: the anchor should be dropped from the
        // inlined backref position but kept at the group definition site.
        // `(^a)\1` — seek = `(?:^a)` (group) + `(?:a)` (backref, anchor dropped)
        assert_eq!(get_seek_pattern(r"(^a)\1"), "(?:^a)(?:a)");
    }

    #[test]
    fn seek_pattern_backref_with_start_anchor_variable_length() {
        // The group has a start anchor plus a variable-length content.
        // Anchor is dropped when inlining for the backref.
        assert_eq!(get_seek_pattern(r"(^a+)\1"), "(?:^a+)(?:a+)");
    }

    #[test]
    fn seek_pattern_backref_with_end_anchor() {
        // Backref to a group ending with `$`: the anchor is dropped in the inline position.
        assert_eq!(get_seek_pattern(r"(a$)\1"), "(?:a$)(?:a)");
    }

    #[test]
    fn seek_pattern_backref_only_anchor() {
        // Backref to a group that is only a positional anchor.
        // The group emits `^` (min_size=0); the backref drops `^` and emits nothing.
        assert_eq!(get_seek_pattern(r"(^)\1"), "^");
    }

    #[test]
    fn seek_pattern_lookahead_dropped() {
        // Lookahead is zero-width and is dropped; only the literal is kept.
        assert_eq!(get_seek_pattern(r"(?=foo)bar"), "bar");
    }

    #[test]
    fn seek_pattern_casei_literal_backref_with_anchor() {
        // Group contains a case-insensitive literal and a start anchor.
        // The anchor is dropped during backref inlining; the casei literal emits (?i:...).
        assert_eq!(get_seek_pattern(r"(?i:(^a))\1"), "(?:^(?i:a))(?:(?i:a))");
    }

    #[test]
    fn seek_pattern_easy_expr_preserved() {
        // An easy (non-hard) expression is serialised verbatim via to_str.
        assert_eq!(get_seek_pattern(r"abc"), "abc");
        assert_eq!(get_seek_pattern(r"a|b"), "a|b");
        assert_eq!(get_seek_pattern(r"a+b*c?"), "a+b*c?");
    }

    #[test]
    fn seek_pattern_end_text_ignore_trailing_newlines_non_crlf() {
        // \Z (non-CRLF mode) — approximate with `\n*$` so the seek only visits positions
        // near end-of-text.
        assert_eq!(get_seek_pattern(r"abc\Z"), r"abc\n*$");
    }

    #[test]
    fn seek_pattern_end_text_ignore_trailing_newlines_crlf() {
        // \Z in CRLF mode — approximate with `(?:\r?\n)*$`.
        assert_eq!(get_seek_pattern(r"(?R)abc\Z"), r"abc(?:\r?\n)*$");
    }

    #[test]
    fn seek_pattern_end_text_ignore_trailing_newlines_only() {
        // \Z alone (hard assertion) — just the seek pattern with no surrounding literal.
        assert_eq!(get_seek_pattern(r"\Z"), r"\n*$");
    }

    #[test]
    fn seek_pattern_backref_with_end_text_ignore_trailing_newlines() {
        // Backref to a group that ends with \Z: \Z is a positional anchor and should be
        // dropped when inlining the group body for the backref.
        // `(a\Z)\1` — seek = `(?:a\n*$)` (group) + `(?:a)` (backref, \Z dropped)
        assert_eq!(get_seek_pattern(r"(a\Z)\1"), r"(?:a\n*$)(?:a)");
    }
}
