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

//! Compiler implementation for compiling regex to VM instructions.

use alloc::boxed::Box;
use alloc::string::{String, ToString};
#[cfg(feature = "variable-lookbehinds")]
use alloc::sync::Arc;
use alloc::vec::Vec;
#[cfg(feature = "variable-lookbehinds")]
use regex_automata::util::pool::Pool;

use crate::analyze::Info;
#[cfg(feature = "variable-lookbehinds")]
use crate::vm::{CachePoolFn, ReverseBackwardsDelegate};
use crate::vm::{CaptureGroupRange, Delegate, Insn};
use crate::LookAround::*;
use crate::{BacktrackingControlVerb, CompileError, Error, Expr, LookAround, RegexOptions, Result};

use super::compile_inner;
use super::vm_builder::VMBuilder;

pub(super) struct Compiler {
    pub(super) b: VMBuilder,
    pub(super) options: RegexOptions,
    pub(super) inside_alternation: bool,
}

impl Compiler {
    pub(super) fn new(max_group: usize) -> Compiler {
        Compiler {
            b: VMBuilder::new(max_group),
            options: Default::default(),
            inside_alternation: false,
        }
    }

    pub(super) fn visit(&mut self, info: &Info<'_>, hard: bool) -> Result<()> {
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
            Expr::Any { newline: true } => {
                self.b.add(Insn::Any);
            }
            Expr::Any { newline: false } => {
                self.b.add(Insn::AnyNoNL);
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
                self.b.add(Insn::Save(group * 2));
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
            Expr::BackrefExistsCondition(group) => {
                self.b.add(Insn::BackrefExistsCondition(group));
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
            Expr::SubroutineCall(_) => {
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported("Subroutine Call".to_string()),
                )));
            }
            Expr::UnresolvedNamedSubroutineCall { .. } => unreachable!(),
            Expr::BackrefWithRelativeRecursionLevel { .. } => unreachable!(),
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
                    let pattern = &delegate_builder.re;
                    let capture_groups = delegate_builder
                        .capture_groups
                        .expect("Expected at least one expression");

                    // Use reverse matching for variable-sized lookbehinds without fancy features
                    use regex_automata::nfa::thompson;
                    // Build a reverse DFA for the pattern
                    let dfa = match regex_automata::hybrid::dfa::DFA::builder()
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
                    let forward_regex = if inner.start_group() != inner.end_group() {
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
                #[cfg(not(feature = "variable-lookbehinds"))]
                {
                    Err(Error::CompileError(Box::new(
                        CompileError::VariableLookBehindRequiresFeature,
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
        } else {
            self.visit(inner, false)
        }
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
}

// Unlike Regex in `regex`, `regex-automata` does not store the pattern string,
// and we cannot retrieve the pattern string using `as_str`.
// Unfortunately we need to get the pattern string in our tests,
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
        let capture_groups = self
            .capture_groups
            .expect("Expected at least one expression");

        let compiled = compile_inner(&self.re, options)?;

        Ok(Insn::Delegate(Delegate {
            inner: compiled,
            pattern: self.re.clone(),
            capture_groups: capture_groups.to_option_if_non_empty(),
        }))
    }
}
