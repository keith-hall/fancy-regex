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

//! Analysis of regex expressions.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use bit_set::BitSet;

use crate::parse::ExprTree;
use crate::vm::CaptureGroupRange;
use crate::{CompileError, Error, Expr, Result};

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

mod analyzer;

pub(crate) use analyzer::Analyzer;

#[derive(Debug)]
pub struct Info<'a> {
    pub(crate) capture_groups: CaptureGroupRange,
    pub(crate) min_size: usize,
    pub(crate) const_size: bool,
    /// Tracks the minimum number of characters that would be consumed in the innermost capture group
    /// before this expression is matched.
    pub(crate) min_pos_in_group: usize,
    pub(crate) hard: bool,
    pub(crate) expr: &'a Expr,
    pub(crate) children: Vec<Info<'a>>,
}

impl<'a> Info<'a> {
    /// Returns the start (first) group number for this expression.
    pub(crate) fn start_group(&self) -> usize {
        self.capture_groups.start()
    }

    /// Returns the end (last) group number for this expression.
    pub(crate) fn end_group(&self) -> usize {
        self.capture_groups.end()
    }

    pub(crate) fn is_literal(&self) -> bool {
        match *self.expr {
            Expr::Literal { casei, .. } => !casei,
            Expr::Concat(_) => self.children.iter().all(|child| child.is_literal()),
            _ => false,
        }
    }

    pub(crate) fn push_literal(&self, buf: &mut String) {
        match *self.expr {
            // could be more paranoid about checking casei
            Expr::Literal { ref val, .. } => buf.push_str(val),
            Expr::Concat(_) => {
                for child in &self.children {
                    child.push_literal(buf);
                }
            }
            _ => panic!("push_literal called on non-literal"),
        }
    }
}

/// Analyze the parsed expression to determine whether it requires fancy features.
pub fn analyze<'a>(tree: &'a ExprTree, explicit_capture_group_0: bool) -> Result<Info<'a>> {
    let start_group = if explicit_capture_group_0 { 0 } else { 1 };
    let mut analyzer = Analyzer {
        backrefs: &tree.backrefs,
        group_ix: start_group,
        group_info: Map::new(),
        subroutine_calls: Map::new(),
        current_group: 0, // Always start at group 0 (the implicit whole-pattern group)
        inside_zero_rep: false,
        root_groups: BitSet::new(),
        contains_forward_referenced_subroutines: false,
    };

    let analyzed = analyzer.visit(&tree.expr, 0);
    if analyzer.backrefs.contains(0) {
        return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
            0,
        ))));
    }
    if let Some(highest_backref) = analyzer.backrefs.into_iter().last() {
        if highest_backref > analyzer.group_ix - start_group
            // if we have an explicit capture group 0, and the highest backref is the number of capture groups
            // then that backref refers to an invalid group
            // i.e. `(a\1)b`   has no capture group 1
            //      `(a(b))\2` has no capture group 2
            || highest_backref == analyzer.group_ix && start_group == 0
        {
            return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
                highest_backref,
            ))));
        }
    }

    // Check for left-recursive subroutine calls (only if subroutines are present)
    if tree.contains_subroutines {
        if let Ok(analyzed_ref) = analyzed.as_ref() {
            if analyzer.contains_forward_referenced_subroutines {
                // Clear and rebuild subroutine_calls with correct positions
                // This is necessary because forward references may have caused incorrect positions
                analyzer.subroutine_calls.clear();
                analyzer.rebuild_subroutine_calls(analyzed_ref, 0, false);
            }
            analyzer.check_left_recursion(&tree.named_groups)?;
        }
    }

    analyzed
}

/// Determine if the expression will always only ever match at position 0.
/// Note that false negatives are possible - it can return false even if it could be anchored.
/// This should therefore only be treated as an optimization.
pub fn can_compile_as_anchored(root_expr: &Expr) -> bool {
    use crate::Assertion;

    match root_expr {
        Expr::Concat(children) => match children[0] {
            Expr::Assertion(assertion) => assertion == Assertion::StartText,
            _ => false,
        },
        Expr::Assertion(assertion) => *assertion == Assertion::StartText,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::analyze;
    // use super::literal_const_size;
    use crate::{can_compile_as_anchored, CompileError, Error, Expr};

    // #[test]
    // fn case_folding_safe() {
    //     let re = regex::Regex::new("(?i:ß)").unwrap();
    //     if re.is_match("SS") {
    //         assert!(!literal_const_size("ß", true));
    //     }

    //     // Another tricky example, Armenian ECH YIWN
    //     let re = regex::Regex::new("(?i:\\x{0587})").unwrap();
    //     if re.is_match("\u{0565}\u{0582}") {
    //         assert!(!literal_const_size("\u{0587}", true));
    //     }
    // }

    #[test]
    fn invalid_backref_zero() {
        let tree = Expr::parse_tree(r".\0").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let tree = Expr::parse_tree(r"(.)\0").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));

        let tree = Expr::parse_tree(r"(.)\0\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(0))
        ));
    }

    #[test]
    fn invalid_backref_no_captures() {
        let tree = Expr::parse_tree(r"aa\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(1))
        ));

        let tree = Expr::parse_tree(r"aaaa\2").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn invalid_backref_with_captures() {
        let tree = Expr::parse_tree(r"a(a)\2").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"a(a)\2\1").unwrap();
        let result = analyze(&tree, false);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn invalid_backref_with_captures_explict_capture_group_zero() {
        let tree = Expr::parse_tree(r"(a(b)\2)c").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"(a(b)\1\2)c").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));

        let tree = Expr::parse_tree(r"(a\1)b").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(1))
        ));

        let tree = Expr::parse_tree(r"(a(b))\2").unwrap();
        let result = analyze(&tree, true);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::InvalidBackref(2))
        ));
    }

    #[test]
    fn allow_analysis_of_self_backref() {
        // even if it will never match, see issue 103
        assert!(!analyze(&Expr::parse_tree(r"(.\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.\1))").unwrap(), true).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+)\1b)").unwrap(), false).is_err());
        // in the following scenario it can match
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+?)(?(1)\1| )c)+").unwrap(), false).is_err());
    }

    #[test]
    fn allow_backref_even_when_capture_group_occurs_after_backref() {
        assert!(!analyze(&Expr::parse_tree(r"\1(.)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(\1(.))").unwrap(), true).is_err());
    }

    #[test]
    fn valid_backref_occurs_after_capture_group() {
        assert!(!analyze(&Expr::parse_tree(r"(.)\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.)\1)").unwrap(), true).is_err());

        assert!(!analyze(&Expr::parse_tree(r"((.)\2\2)\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)\1(.)\2").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)foo(.)\2").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\2\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\1").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\2\1").unwrap(), false).is_err());
    }

    #[test]
    fn feature_not_yet_supported() {
        // Subroutine calls are now supported in analysis, so (a)\g<1> should work
        let tree = &Expr::parse_tree(r"(a)\g<1>").unwrap();
        let result = analyze(tree, false);
        assert!(result.is_ok()); // Changed: subroutine calls are now analyzed

        // Backref with relative recursion level is still not supported
        let tree = &Expr::parse_tree(r"(a)\k<1-0>").unwrap();
        let result = analyze(tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::FeatureNotYetSupported(_))
        ));
    }

    #[test]
    fn subroutine_call_undefined() {
        let tree = &Expr::parse_tree(r"\g<wrong_name>(?<different_name>a)").unwrap();
        let result = analyze(tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::SubroutineCallTargetNotFound(_, _))
        ));
    }

    #[test]
    fn is_literal() {
        let tree = Expr::parse_tree("abc").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.is_literal(), true);
    }

    #[test]
    fn is_literal_with_repeat() {
        let tree = Expr::parse_tree("abc*").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.is_literal(), false);
    }

    #[test]
    fn anchored_for_starttext_assertions() {
        let tree = Expr::parse_tree(r"^(\w+)\1").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), true);

        let tree = Expr::parse_tree(r"^").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), true);
    }

    #[test]
    fn backref_inherits_group_size_info() {
        // Test that backrefs properly inherit min_size and const_size from referenced groups
        let tree = Expr::parse_tree(r"(abc)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // The concatenation should have min_size = 3 + 3 = 6 (group + backref)
        assert_eq!(info.min_size, 6);
        assert!(info.const_size);

        // Test with a variable-length group
        let tree = Expr::parse_tree(r"(a+)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // The group has min_size = 1, but const_size = false due to the +
        // So the total should be min_size = 2, const_size = false
        assert_eq!(info.min_size, 2);
        assert!(!info.const_size);

        // Test with optional group
        let tree = Expr::parse_tree(r"(a?)\1").unwrap();
        let info = analyze(&tree, false).unwrap();
        // Both group and backref can be empty, so min_size = 0
        assert_eq!(info.min_size, 0);
        assert!(!info.const_size);
    }

    #[test]
    fn backref_forward_reference() {
        // Test forward references (backref before group definition)
        // These should use conservative defaults but still work
        let tree = Expr::parse_tree(r"\1(abc)").unwrap();
        let info = analyze(&tree, false).unwrap();
        // Forward ref gets min_size=0, group gets min_size=3, total=3
        assert_eq!(info.min_size, 3);
        // Forward ref sets const_size=false, so overall is false
        assert!(!info.const_size);
    }

    #[test]
    fn backref_in_lookbehind() {
        assert!(!analyze(&Expr::parse_tree(r"(hello)(?<=\b\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(..)(?<=\1\1)").unwrap(), false).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(abc)(?<=\1)def").unwrap(), false).is_err());
    }

    #[test]
    fn not_anchored_for_startline_assertions() {
        let tree = Expr::parse_tree(r"(?m)^(\w+)\1").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), false);
    }

    #[test]
    fn min_pos_in_group_calculated_correctly_with_no_groups() {
        let tree = Expr::parse_tree(r"\G").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(info.const_size);

        let tree = Expr::parse_tree(r"\G(?=abc)\w+").unwrap();
        let info = analyze(&tree, false).unwrap();
        // the lookahead itself has min size 0
        assert_eq!(info.children[1].min_size, 0);
        assert!(info.children[1].const_size);
        // the children of the lookahead have min_size 3 from the literal
        assert_eq!(info.children[1].children[0].min_size, 3);
        assert!(info.children[1].children[0].const_size);
        // after lookahead, the position is reset
        assert_eq!(info.children[2].min_pos_in_group, 0);
        assert_eq!(info.children[2].min_size, 1);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(!info.const_size);

        let tree = Expr::parse_tree(r"(?:ab*|cd){2}(?=bar)\w").unwrap();
        let info = analyze(&tree, false).unwrap();
        // the whole expression has min size 3 (a times 2 plus \w)
        assert_eq!(info.min_size, 3);
        // the min pos of the lookahead is 2
        assert_eq!(info.children[1].min_pos_in_group, 2);
        // after lookahead, the position is reset
        assert_eq!(info.children[2].min_pos_in_group, 2);
        assert_eq!(info.children[2].min_size, 1);
        assert!(!info.const_size);
    }

    #[test]
    fn backtracking_control_verb_is_hard_and_const_size() {
        let tree = Expr::parse_tree(r"(*FAIL)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_size, 0);
        assert_eq!(info.min_pos_in_group, 0);
        assert!(info.const_size);
    }

    #[test]
    fn min_pos_in_group_calculated_correctly_with_capture_groups() {
        use matches::assert_matches;

        let tree = Expr::parse_tree(r"a(bc)d(e(f)g)").unwrap();
        let info = analyze(&tree, false).unwrap();
        assert_eq!(info.min_pos_in_group, 0);
        // before the capture begins, the min pos in group 0 is 1
        assert_eq!(info.children[1].min_pos_in_group, 1);
        // inside capture group 1, the min pos of the Concat inside the group is 0
        assert_matches!(info.children[1].children[0].expr, Expr::Concat(_));
        assert_eq!(info.children[1].children[0].min_pos_in_group, 0);
        assert!(info.children[1].children[0].const_size);
        // inside capture group 1, the min pos of the c inside the group is 1
        assert_matches!(info.children[1].children[0].children[1].expr, Expr::Literal { val, casei: false } if val == "c");
        assert_eq!(info.children[1].children[0].children[1].min_pos_in_group, 1);

        // prove we are looking at the position of the d after capture group 1
        assert_matches!(info.children[2].expr, Expr::Literal { val, casei: false } if val == "d");
        assert_eq!(info.children[2].min_pos_in_group, 3);
        assert_eq!(info.children[2].start_group(), 2);
        assert_eq!(info.children[2].min_size, 1);

        // prove we are looking at the position of the e in capture group 2
        assert_matches!(info.children[3].children[0].children[0].expr, Expr::Literal { val, casei: false } if val == "e");
        assert_eq!(info.children[3].children[0].children[0].min_pos_in_group, 0);
    }

    #[test]
    fn left_recursive_subroutine_direct() {
        // Direct left recursion: group 1 calls itself at position 0
        let tree = Expr::parse_tree(r"(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"abc(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn not_left_recursive_subroutine_after_group() {
        let tree = Expr::parse_tree(r"(a)\g<1>").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());

        let tree = Expr::parse_tree(r"(?<test>a)\g<test>").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_subroutine_at_start() {
        // Left recursion at start of group: (\g<1>a)
        let tree = Expr::parse_tree(r"(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"(?<test>\g<test>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_subroutine_indirect() {
        // Indirect left recursion: non-nested subroutine calls to each other
        let tree = Expr::parse_tree(r"(\g<2>)(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));

        let tree = Expr::parse_tree(r"(\g<2>)(\g<1>a)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_subroutine_with_alternation() {
        // Left recursion through alternation, depending which branch is taken it could be left-recursive
        let tree = Expr::parse_tree(r"(a|\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn not_left_recursive_after_char() {
        // Not left recursive because subroutine call is after a character was consumed
        let tree = Expr::parse_tree(r"(a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn not_left_recursive_zero_repetition() {
        // Not left recursive because subroutine call is unreachable
        let tree = Expr::parse_tree(r"(a?\g<1>){0}").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_with_both_positions() {
        // Left recursive because \g<1> appears at position 0 in the group even though also at end at position 1
        let tree = Expr::parse_tree(r"(\g<1>a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn left_recursive_with_lookahead() {
        let tree = Expr::parse_tree(r"((?=a)\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn self_recursive_group_zero() {
        // Self-recursive on group 0 after a character
        let tree = Expr::parse_tree(r"a\g<0>").unwrap();
        let result = analyze(&tree, false);
        // Group 0 calls itself at position 1 (after 'a'), so this is NOT left recursive
        assert!(result.is_ok());
    }

    #[test]
    fn not_left_recursive_forward_call() {
        // Forward subroutine call - not left recursive: \g<1>(a)
        let tree = Expr::parse_tree(r"\g<1>(a)").unwrap();
        let result = analyze(&tree, false);
        // The call happens before the group is defined, but it's at position 0 of group 0 (implicit)
        // which calls group 1. Group 1 doesn't call anything, so no cycle.
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_group_zero_explicit() {
        // Self-recursive on explicit group 0: (a\g<0>)
        let tree = Expr::parse_tree(r"(a\g<0>)").unwrap();
        let result = analyze(&tree, true);
        assert!(result.is_ok());
    }

    #[test]
    fn left_recursive_group_zero_at_start() {
        // Self-recursive on explicit group 0 at start: (\g<0>a)
        let tree = Expr::parse_tree(r"(\g<0>a)").unwrap();
        let result = analyze(&tree, true);
        // With explicit group 0, \g<0> at position 0 is left-recursive
        assert!(result.is_err());
    }

    #[test]
    fn three_way_indirect_recursion() {
        // Three-way indirect recursion
        let tree = Expr::parse_tree(r"(\g<2>)(\g<3>)(a\g<1>)").unwrap();
        let result = analyze(&tree, false);
        // Group 1 -> Group 2 (at pos 0)
        // Group 2 -> Group 3 (at pos 0)
        // Group 3 -> Group 1 (at pos 1, after 'a')
        // This forms a cycle, but the call from group 3 to group 1 is at position 1
        // So it's not left-recursive
        assert!(result.is_ok());
    }

    #[test]
    fn three_way_left_recursive() {
        // Three-way left recursion
        let tree = Expr::parse_tree(r"(\g<2>)(\g<3>)(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        // Group 1 -> Group 2 (at pos 0)
        // Group 2 -> Group 3 (at pos 0)
        // Group 3 -> Group 1 (at pos 0)
        // This forms a left-recursive cycle
        assert!(result.is_err());

        let tree = Expr::parse_tree(r"(\g<2>a)(\g<3>b)(\g<1>c)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err());
    }

    #[test]
    fn left_recursive_with_call_to_defined_group() {
        // Even though the call from Group 1 to Group 2 is inside {0} at root level,
        // Group 1's pattern can still be executed when called from Group 2
        // Group 1 contains a?\g<2> - calls group 2 (when executed)
        // Group 2 contains \g<1> - calls group 1 at position 0
        // This creates a cycle: Group 2 -> Group 1 (at pos 0) -> Group 2 (at pos 0)
        let tree = Expr::parse_tree(r"(a?\g<2>){0}(\g<1>)").unwrap();
        let result = analyze(&tree, false);
        assert!(result.is_err(), "Should be left-recursive");
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(ref box_err)) if matches!(**box_err, CompileError::LeftRecursiveSubroutineCall(_))
        ));
    }

    #[test]
    fn no_left_recursion_complex_pattern() {
        // Group n (1): |\g<m>\g<n> - calls m then itself, but m has min_size > 0
        // Group m (2): a(b)\g<m> - calls itself after 'ab'
        let tree = Expr::parse_tree(r"(?<n>|\g<m>\g<n>)\z|\zEND (?<m>a(b)\g<m>)").unwrap();
        let result = analyze(&tree, false);

        assert!(
            result.is_ok(),
            "Pattern should not be left-recursive because group m has min_size > 0"
        );
    }
}
