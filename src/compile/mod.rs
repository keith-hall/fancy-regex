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
use regex_automata::meta::Regex as RaRegex;
use regex_automata::meta::{Builder as RaBuilder, Config as RaConfig};

use crate::analyze::Info;
#[cfg(feature = "variable-lookbehinds")]
use crate::vm::ReverseBackwardsDelegate;
use crate::vm::{CaptureGroupRange, Delegate, Insn, Prog};
use crate::{CompileError, Error, Expr, RegexOptions, Result};

mod compiler;
mod vm_builder;

use compiler::Compiler;

// Global pattern mapping for test diagnostics
#[cfg(all(test, feature = "std"))]
use std::{collections::BTreeMap, sync::RwLock};
#[cfg(all(test, feature = "std"))]
static PATTERN_MAPPING: RwLock<BTreeMap<String, String>> = RwLock::new(BTreeMap::new());

pub(crate) fn compile_inner(inner_re: &str, options: &RegexOptions) -> Result<RaRegex> {
    let mut config = RaConfig::new();
    if let Some(size_limit) = options.delegate_size_limit {
        config = config.nfa_size_limit(Some(size_limit));
    }
    if let Some(dfa_size_limit) = options.delegate_dfa_size_limit {
        config = config.dfa_size_limit(Some(dfa_size_limit));
    }

    let re = RaBuilder::new()
        .configure(config)
        .syntax(options.syntaxc)
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

/// Compile the analyzed expressions into a program.
pub fn compile(info: &Info<'_>, anchored: bool) -> Result<Prog> {
    let mut c = Compiler::new(info.end_group());
    if !anchored {
        // add instructions as if \O*? was used at the start of the expression
        // so that we bump the haystack index by one when failing to match at the current position
        let current_pc = c.b.pc();
        // we are adding 3 instructions, so the current program counter plus 3 gives us the first real instruction
        c.b.add(Insn::Split(current_pc + 3, current_pc + 1));
        c.b.add(Insn::Any);
        c.b.add(Insn::Jmp(current_pc));
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
                .map(|range: CaptureGroupRange| CaptureGroupRange(range.start(), info.end_group()));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyze::analyze;
    use crate::parse::ExprTree;
    use crate::vm::Insn::*;
    use alloc::vec;
    use bit_set::BitSet;
    use matches::assert_matches;

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
            contains_subroutines: false,
            self_recursive: false,
        };
        let info = analyze(&tree, false).unwrap();

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
        assert_delegate(&prog[1], "ab*");
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
        assert_delegate(&prog[3], "(?:a|ab)x*");
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
        assert_delegate(&prog[3], "(?:a|b)c");
        assert_delegate(&prog[4], "x*");
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
        assert_delegate(&prog[7], "x*");
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
        assert_matches!(prog[3], Save(0));
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
        let info = analyze(&tree, true).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::FeatureNotYetSupported(_))
        );

        let tree = Expr::parse_tree(r"(*COMMIT)").unwrap();
        let info = analyze(&tree, true).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::FeatureNotYetSupported(_))
        );

        let tree = Expr::parse_tree(r"(*SKIP)").unwrap();
        let info = analyze(&tree, true).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::FeatureNotYetSupported(_))
        );

        let tree = Expr::parse_tree(r"(*PRUNE)").unwrap();
        let info = analyze(&tree, true).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::FeatureNotYetSupported(_))
        );
    }

    #[test]
    #[cfg(not(feature = "variable-lookbehinds"))]
    fn variable_lookbehind_requires_feature() {
        // Without the feature flag, variable-length lookbehinds should error
        let tree = Expr::parse_tree(r"(?<=ab+)x").unwrap();
        let info = analyze(&tree, true).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::VariableLookBehindRequiresFeature)
        );
    }

    #[test]
    #[cfg(feature = "variable-lookbehinds")]
    fn variable_lookbehind_with_required_feature_no_captures() {
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
        let info = analyze(&tree, false).unwrap();
        let result = compile(&info, true);
        assert!(result.is_err());
        assert_matches!(
            result.err().unwrap(),
            Error::CompileError(box_err) if matches!(*box_err, CompileError::FeatureNotYetSupported(_))
        );
    }

    fn compile_prog(re: &str) -> Vec<Insn> {
        let tree = Expr::parse_tree(re).unwrap();
        let info = analyze(&tree, true).unwrap();
        let prog = compile(&info, true).unwrap();
        prog.body
    }

    #[cfg(feature = "std")]
    fn assert_delegate(insn: &Insn, re: &str) {
        use crate::vm::Delegate;

        match insn {
            Insn::Delegate(Delegate { inner, .. }) => {
                assert_eq!(
                    PATTERN_MAPPING
                        .read()
                        .unwrap()
                        .get(&alloc::format!("{:?}", inner))
                        .unwrap(),
                    re
                );
            }
            _ => {
                panic!("Expected Insn::Delegate but was {:#?}", insn);
            }
        }
    }

    #[cfg(not(feature = "std"))]
    fn assert_delegate(_: &Insn, _: &str) {}
}
