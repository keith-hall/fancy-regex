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

use alloc::string::String;
use alloc::vec::Vec;
use core::cmp::min;

use bit_set::BitSet;

use crate::alloc::string::ToString;
use crate::parse::ExprTree;
use crate::{CompileError, Error, Expr, Result};

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

#[derive(Debug)]
pub struct Info<'a> {
    pub(crate) start_group: usize,
    pub(crate) end_group: usize,
    pub(crate) min_size: usize,
    pub(crate) const_size: bool,
    pub(crate) hard: bool,
    pub(crate) expr: &'a Expr,
    pub(crate) children: Vec<Info<'a>>,
    pub(crate) min_pos_in_group: usize,
}

impl<'a> Info<'a> {
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

/// Represents a subroutine call in the call graph
#[derive(Debug, Clone)]
struct SubroutineCall {
    /// The group being called
    target: usize,
    /// The minimum position within the calling group where this call occurs
    min_pos: usize,
}

struct Analyzer<'a> {
    backrefs: &'a BitSet,
    group_ix: usize,
    inside_groups: BitSet,
    /// Maps group number -> list of subroutine calls made by that group
    call_graph: Map<usize, Vec<SubroutineCall>>,
    /// Track whether we encountered any subroutine calls that need to be checked
    has_subroutine_calls: bool,
}

impl<'a> Analyzer<'a> {
    /// Detects left-recursive cycles in the call graph using DFS
    fn detect_left_recursive_cycles(&self) -> Result<()> {
        let mut visited = BitSet::new();
        let mut in_stack = BitSet::new();
        
        for group in self.call_graph.keys() {
            if !visited.contains(*group) {
                self.dfs_detect_cycle(*group, &mut visited, &mut in_stack, &mut Vec::new())?;
            }
        }
        
        Ok(())
    }
    
    /// DFS helper for cycle detection
    fn dfs_detect_cycle(
        &self,
        group: usize,
        visited: &mut BitSet,
        in_stack: &mut BitSet,
        path: &mut Vec<(usize, usize)>, // (group, min_pos)
    ) -> Result<()> {
        visited.insert(group);
        in_stack.insert(group);
        
        if let Some(calls) = self.call_graph.get(&group) {
            for call in calls {
                if in_stack.contains(call.target) {
                    // Found a cycle, check if it's left-recursive
                    if self.is_cycle_left_recursive(call, path)? {
                        // Return the group at the start of the cycle (the target, not current group)
                        return Err(Error::CompileError(
                            CompileError::LeftRecursiveSubroutineCall(call.target),
                        ));
                    }
                } else if !visited.contains(call.target) {
                    path.push((group, call.min_pos));
                    self.dfs_detect_cycle(call.target, visited, in_stack, path)?;
                    path.pop();
                }
            }
        }
        
        in_stack.remove(group);
        Ok(())
    }
    
    /// Checks if a cycle is left-recursive by examining call positions
    fn is_cycle_left_recursive(
        &self,
        back_edge: &SubroutineCall,
        path: &[(usize, usize)],
    ) -> Result<bool> {
        // A cycle is left-recursive if any call in the cycle occurs at position 0
        
        // Check the back edge
        if back_edge.min_pos == 0 {
            return Ok(true);
        }
        
        // Check calls in the path leading to the cycle
        let mut cycle_start_idx = None;
        for (i, &(group, _)) in path.iter().enumerate() {
            if group == back_edge.target {
                cycle_start_idx = Some(i);
                break;
            }
        }
        
        if let Some(start_idx) = cycle_start_idx {
            for &(_, min_pos) in &path[start_idx..] {
                if min_pos == 0 {
                    return Ok(true);
                }
            }
        }
        
        Ok(false)
    }

    fn visit(&mut self, expr: &'a Expr, min_pos_in_group: usize) -> Result<Info<'a>> {
        let start_group = self.group_ix;
        let mut children = Vec::new();
        let mut min_size = 0;
        let mut const_size = false;
        let mut hard = false;
        match *expr {
            Expr::Assertion(assertion) if assertion.is_hard() => {
                const_size = true;
                hard = true;
            }
            Expr::Empty | Expr::Assertion(_) => {
                const_size = true;
            }
            Expr::Any { .. } => {
                min_size = 1;
                const_size = true;
            }
            Expr::Literal { ref val, casei } => {
                // right now each character in a literal gets its own node, that might change
                min_size = 1;
                const_size = literal_const_size(val, casei);
            }
            Expr::Concat(ref v) => {
                const_size = true;
                let mut pos_in_group = min_pos_in_group;
                for child in v {
                    let child_info = self.visit(child, pos_in_group)?;
                    min_size += child_info.min_size;
                    const_size &= child_info.const_size;
                    hard |= child_info.hard;
                    pos_in_group += child_info.min_size;
                    children.push(child_info);
                }
            }
            Expr::Alt(ref v) => {
                let child_info = self.visit(&v[0], min_pos_in_group)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = child_info.hard;
                children.push(child_info);
                for child in &v[1..] {
                    let child_info = self.visit(child, min_pos_in_group)?;
                    const_size &= child_info.const_size && min_size == child_info.min_size;
                    min_size = min(min_size, child_info.min_size);
                    hard |= child_info.hard;
                    children.push(child_info);
                }
            }
            Expr::Group(ref child) => {
                let group = self.group_ix;
                self.group_ix += 1;
                self.call_graph.insert(group, Vec::new());
                self.inside_groups.insert(group);
                let child_info = self.visit(child, 0)?;
                self.inside_groups.remove(group);
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                // If there's a backref to this group, we potentially have to backtrack within the
                // group. E.g. with `(x|xy)\1` and input `xyxy`, `x` matches but then the backref
                // doesn't, so we have to backtrack and try `xy`.
                hard = child_info.hard | self.backrefs.contains(group);
                children.push(child_info);
            }
            Expr::LookAround(ref child, _) => {
                let child_info = self.visit(child, min_pos_in_group)?;
                // min_size = 0
                const_size = true;
                hard = true;
                children.push(child_info);
            }
            Expr::Repeat {
                ref child, lo, hi, ..
            } => {
                // For repeat expressions with max = 0, the child doesn't actually contribute to min_size
                // This is important for left recursion detection
                let child_info = if hi == 0 {
                    // If max repetitions is 0, this repeat matches nothing
                    // TODO: find out what group we are actually in, not use the number of groups encountered so far
                    let was_reachable = self.inside_groups.contains(self.group_ix - 1);
                    if was_reachable {
                        self.inside_groups.remove(self.group_ix - 1);
                    }
                    let child_info = self.visit(child, min_pos_in_group)?;
                    if was_reachable {
                        self.inside_groups.insert(self.group_ix - 1);
                    }
                    child_info
                } else {
                    self.visit(child, min_pos_in_group)?
                };
                
                min_size = child_info.min_size * lo;
                const_size = child_info.const_size && lo == hi;
                hard = child_info.hard;
                children.push(child_info);
            }
            Expr::Delegate { size, .. } => {
                // currently only used for empty and single-char matches
                min_size = size;
                const_size = true;
            }
            Expr::Backref { group, .. } => {
                if group == 0 {
                    return Err(Error::CompileError(CompileError::InvalidBackref(group)));
                }
                hard = true;
            }
            Expr::AtomicGroup(ref child) => {
                let child_info = self.visit(child, min_pos_in_group)?;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                hard = true; // TODO: possibly could weaken
                children.push(child_info);
            }
            Expr::KeepOut => {
                hard = true;
                const_size = true;
            }
            Expr::ContinueFromPreviousMatchEnd => {
                hard = true;
                const_size = true;
            }
            Expr::BackrefExistsCondition(_) => {
                hard = true;
                const_size = true;
            }
            Expr::Conditional {
                ref condition,
                ref true_branch,
                ref false_branch,
            } => {
                hard = true;

                let child_info_condition = self.visit(condition, min_pos_in_group)?;
                let child_info_truth = self.visit(true_branch, min_pos_in_group + child_info_condition.min_size)?;
                let child_info_false = self.visit(false_branch, min_pos_in_group)?;

                min_size = child_info_condition.min_size
                    + min(child_info_truth.min_size, child_info_false.min_size);
                const_size = child_info_condition.const_size
                    && child_info_truth.const_size
                    && child_info_false.const_size
                    // if the condition's size plus the truth branch's size is equal to the false branch's size then it's const size
                    && child_info_condition.min_size + child_info_truth.min_size == child_info_false.min_size;

                children.push(child_info_condition);
                children.push(child_info_truth);
                children.push(child_info_false);
            }
            Expr::SubroutineCall(group) => {
                // Record the subroutine call in the call graph
                for inside_group in self.inside_groups.iter() {
                    if let Some(calls) = self.call_graph.get_mut(&inside_group) {
                        calls.push(SubroutineCall {
                            target: group,
                            min_pos: min_pos_in_group,
                        });
                    }
                }
                
                self.has_subroutine_calls = true;

                // Check for direct left recursive calls immediately if we're at position 0
                if min_pos_in_group == 0 && self.inside_groups.contains(group) {
                    return Err(Error::CompileError(
                        CompileError::LeftRecursiveSubroutineCall(group),
                    ));
                }

                // Continue analysis - don't return error yet so we can detect all cycles
                hard = true;
            }
            Expr::UnresolvedNamedSubroutineCall { ref name, ix } => {
                return Err(Error::CompileError(
                    CompileError::SubroutineCallTargetNotFound(name.to_string(), ix),
                ));
            }
            Expr::BackrefWithRelativeRecursionLevel { .. } => {
                return Err(Error::CompileError(CompileError::FeatureNotYetSupported(
                    "Backref at recursion level".to_string(),
                )));
            }
        };

        Ok(Info {
            expr,
            children,
            start_group,
            end_group: self.group_ix,
            min_size,
            const_size,
            hard,
            min_pos_in_group,
        })
    }
}

fn literal_const_size(_: &str, _: bool) -> bool {
    // Right now, regex doesn't do sophisticated case folding,
    // test below will fail when that changes, then we need to
    // do something fancier here.
    true
}

/// Analyze the parsed expression to determine whether it requires fancy features.
pub fn analyze<'a>(tree: &'a ExprTree, start_group: usize) -> Result<Info<'a>> {
    let mut analyzer = Analyzer {
        backrefs: &tree.backrefs,
        group_ix: start_group,
        call_graph: Map::new(),
        inside_groups: BitSet::new(),
        has_subroutine_calls: false,
    };

    let analyzed = analyzer.visit(&tree.expr, 0)?;
    
    // Perform DFS-based cycle detection for left recursion
    analyzer.detect_left_recursive_cycles()?;
    
    // If we have subroutine calls but no left recursion detected, return not supported error
    if analyzer.has_subroutine_calls {
        return Err(Error::CompileError(CompileError::FeatureNotYetSupported(
            "Subroutine Call".to_string(),
        )));
    }
    
    if analyzer.backrefs.contains(0) {
        return Err(Error::CompileError(CompileError::InvalidBackref(0)));
    }
    if let Some(highest_backref) = analyzer.backrefs.into_iter().last() {
        if highest_backref > analyzer.group_ix - start_group
            // if we have an explicit capture group 0, and the highest backref is the number of capture groups
            // then that backref refers to an invalid group
            // i.e. `(a\1)b`   has no capture group 1
            //      `(a(b))\2` has no capture group 2
            || highest_backref == analyzer.group_ix && start_group == 0
        {
            return Err(Error::CompileError(CompileError::InvalidBackref(
                highest_backref,
            )));
        }
    }
    Ok(analyzed)
}

/// Determine if the expression will always only ever match at position 0.
/// Note that false negatives are possible - it can return false even if it could be anchored.
/// This should therefore only be treated as an optimization.
pub fn can_compile_as_anchored(root_expr: &Expr) -> bool {
    use crate::Assertion;

    match root_expr {
        Expr::Concat(ref children) => match children[0] {
            Expr::Assertion(ref assertion) => *assertion == Assertion::StartText,
            _ => false,
        },
        Expr::Assertion(ref assertion) => *assertion == Assertion::StartText,
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
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(0)))
        ));

        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(0)))
        ));

        let tree = Expr::parse_tree(r"(.)\0").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(0)))
        ));

        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(0)))
        ));

        let tree = Expr::parse_tree(r"(.)\0\1").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(0)))
        ));
    }

    #[test]
    fn invalid_backref_no_captures() {
        let tree = Expr::parse_tree(r"aa\1").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(1)))
        ));

        let tree = Expr::parse_tree(r"aaaa\2").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));
    }

    #[test]
    fn invalid_backref_with_captures() {
        let tree = Expr::parse_tree(r"a(a)\2").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));

        let tree = Expr::parse_tree(r"a(a)\2\1").unwrap();
        let result = analyze(&tree, 1);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));
    }

    #[test]
    fn invalid_backref_with_captures_explict_capture_group_zero() {
        let tree = Expr::parse_tree(r"(a(b)\2)c").unwrap();
        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));

        let tree = Expr::parse_tree(r"(a(b)\1\2)c").unwrap();
        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));

        let tree = Expr::parse_tree(r"(a\1)b").unwrap();
        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(1)))
        ));

        let tree = Expr::parse_tree(r"(a(b))\2").unwrap();
        let result = analyze(&tree, 0);
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::InvalidBackref(2)))
        ));
    }

    #[test]
    fn allow_analysis_of_self_backref() {
        // even if it will never match, see issue 103
        assert!(!analyze(&Expr::parse_tree(r"(.\1)").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.\1))").unwrap(), 0).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+)\1b)").unwrap(), 1).is_err());
        // in the following scenario it can match
        assert!(!analyze(&Expr::parse_tree(r"(([ab]+?)(?(1)\1| )c)+").unwrap(), 1).is_err());
    }

    #[test]
    fn allow_backref_even_when_capture_group_occurs_after_backref() {
        assert!(!analyze(&Expr::parse_tree(r"\1(.)").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(\1(.))").unwrap(), 0).is_err());
    }

    #[test]
    fn valid_backref_occurs_after_capture_group() {
        assert!(!analyze(&Expr::parse_tree(r"(.)\1").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"((.)\1)").unwrap(), 0).is_err());

        assert!(!analyze(&Expr::parse_tree(r"((.)\2\2)\1").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)\1(.)\2").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)foo(.)\2").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\2\1").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\3\1").unwrap(), 1).is_err());
        assert!(!analyze(&Expr::parse_tree(r"(.)(foo)(.)\2\1").unwrap(), 1).is_err());
    }

    #[test]
    fn feature_not_yet_supported() {
        let tree = &Expr::parse_tree(r"(a)\g<1>").unwrap();
        let result = analyze(tree, 1);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::FeatureNotYetSupported(_)))
        ));

        let tree = &Expr::parse_tree(r"(a)\k<1-0>").unwrap();
        let result = analyze(tree, 1);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::FeatureNotYetSupported(_)))
        ));
    }

    #[test]
    fn subroutine_call_undefined() {
        let tree = &Expr::parse_tree(r"\g<wrong_name>(?<different_name>a)").unwrap();
        let result = analyze(tree, 1);
        assert!(result.is_err());
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(
                CompileError::SubroutineCallTargetNotFound(_, _)
            ))
        ));
    }

    #[test]
    fn left_recursive_subroutine_call() {
        let tree = Expr::parse_tree(r"(?<a>\g<a>)").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        assert!(matches!(result, Some(Error::CompileError(CompileError::LeftRecursiveSubroutineCall(1)))));

        let tree = Expr::parse_tree(r"(?<a>(?:\g<a>|))").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        assert!(matches!(result, Some(Error::CompileError(CompileError::LeftRecursiveSubroutineCall(1)))));

        let tree = Expr::parse_tree(r"(?<name>a|\g<name>b)").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        assert!(matches!(result, Some(Error::CompileError(CompileError::LeftRecursiveSubroutineCall(1)))));
    }

    #[test]
    fn indirect_left_recursive_subroutine_call() {
        let tree = Expr::parse_tree(r"(?<a>(?=\g<b>|))(?<b>\g<a>)").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        assert!(matches!(result, Some(Error::CompileError(CompileError::LeftRecursiveSubroutineCall(1)))));
    }

    #[test]
    fn repeat_with_max_zero_ignored_for_left_recursion() {
        let tree = Expr::parse_tree(r"(?<a>\g<a>{0})").unwrap();
        let result = analyze(&tree, 1);
        assert!(!result.is_err());

        let tree = Expr::parse_tree(r"(?<a>x{0}\g<a>)").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        // Should detect left recursion because x{0} doesn't consume anything
        assert!(matches!(result, Some(Error::CompileError(CompileError::LeftRecursiveSubroutineCall(1)))));

        let tree = Expr::parse_tree(r"(?<a>(?:\g<a>){0})").unwrap();
        let result = analyze(&tree, 1);
        assert!(!result.is_err());
    }

    #[test]
    fn recursive_subroutine_call_not_yet_supported() {
        let tree = Expr::parse_tree(r"(a\g<1>)").unwrap();
        let result = analyze(&tree, 1).err();
        println!("{:?}", result);
        //let expected = "Recursive Subroutine Call".to_string();
        assert!(matches!(
            result,
            Some(Error::CompileError(CompileError::FeatureNotYetSupported(_)))
        ));
    }

    #[test]
    fn less_obvious_recursive_subroutine_call_not_yet_supported() {
        let tree = Expr::parse_tree(r"(?<a>a\g<b>)(?<b>b\g<a>?)").unwrap();
        let result = analyze(&tree, 1);
        println!("{:?}", result);
        //let expected = "Recursive Subroutine Call".to_string();
        assert!(matches!(
            result.err(),
            Some(Error::CompileError(CompileError::FeatureNotYetSupported(_)))
        ));
    }

    #[test]
    fn is_literal() {
        let tree = Expr::parse_tree("abc").unwrap();
        let info = analyze(&tree, 1).unwrap();
        assert_eq!(info.is_literal(), true);
    }

    #[test]
    fn is_literal_with_repeat() {
        let tree = Expr::parse_tree("abc*").unwrap();
        let info = analyze(&tree, 1).unwrap();
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
    fn not_anchored_for_startline_assertions() {
        let tree = Expr::parse_tree(r"(?m)^(\w+)\1").unwrap();
        assert_eq!(can_compile_as_anchored(&tree.expr), false);
    }
}
