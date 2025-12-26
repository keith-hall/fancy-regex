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

//! Analyzer implementation for regex analysis.

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::cmp::min;

use bit_set::BitSet;

use crate::alloc::string::ToString;
use crate::vm::CaptureGroupRange;
use crate::{CompileError, Error, Expr, Result};

use super::{Info, Map};

pub(super) struct SizeInfo {
    pub min_size: usize,
    pub const_size: bool,
}

/// Represents a subroutine call and its minimum position within a group
#[derive(Debug, Clone)]
pub(super) struct SubroutineCallInfo {
    /// The group being called
    pub target_group: usize,
    /// The minimum number of characters consumed in the haystack by the capture group in which this call occurs
    pub min_pos: usize,
}

pub(crate) struct Analyzer<'a> {
    pub(super) backrefs: &'a BitSet,
    pub(super) group_ix: usize,
    /// Stores the analysis info for each group by group number
    // NOTE: uses a Map instead of a Vec because sometimes we start from capture group 1, other times 0
    pub(super) group_info: Map<usize, SizeInfo>,
    /// Tracks subroutine calls: maps from a group to the subroutines it calls
    pub(super) subroutine_calls: Map<usize, Vec<SubroutineCallInfo>>,
    /// The current group being analyzed (for tracking which group contains subroutine calls)
    pub(super) current_group: usize,
    /// Whether we're currently inside a zero-repetition (unreachable code)
    pub(super) inside_zero_rep: bool,
    /// Groups that are directly executed from root (not inside {0})
    pub(super) root_groups: BitSet,
    /// Contains subroutine calls to groups which weren't analyzed yet at the time of the call
    pub(super) contains_forward_referenced_subroutines: bool,
}

impl<'a> Analyzer<'a> {
    pub(super) fn visit(&mut self, expr: &'a Expr, min_pos_in_group: usize) -> Result<Info<'a>> {
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
                let prev_group = self.current_group;
                self.current_group = group;
                let child_info = self.visit(child, 0)?;
                self.current_group = prev_group;
                min_size = child_info.min_size;
                const_size = child_info.const_size;
                // Store the group info for use by backrefs
                self.group_info.insert(
                    group,
                    SizeInfo {
                        min_size,
                        const_size,
                    },
                );
                // If there's a backref to this group, we potentially have to backtrack within the
                // group. E.g. with `(x|xy)\1` and input `xyxy`, `x` matches but then the backref
                // doesn't, so we have to backtrack and try `xy`.
                hard = child_info.hard | self.backrefs.contains(group);
                children.push(child_info);
            }
            Expr::LookAround(ref child, _) => {
                // NOTE: min_pos_in_group might seem weird for lookbehinds
                let child_info = self.visit(child, min_pos_in_group)?;
                // min_size = 0
                const_size = true;
                hard = true;
                children.push(child_info);
            }
            Expr::Repeat {
                ref child, lo, hi, ..
            } => {
                // If lo and hi are both 0, we're in a zero-repetition (unreachable)
                let prev_zero_rep = self.inside_zero_rep;
                if lo == 0 && hi == 0 {
                    self.inside_zero_rep = true;
                }
                let child_info = self.visit(child, min_pos_in_group)?;
                self.inside_zero_rep = prev_zero_rep;
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
                    return Err(Error::CompileError(Box::new(CompileError::InvalidBackref(
                        group,
                    ))));
                }
                // Look up the referenced group's size information
                if let Some(&SizeInfo {
                    min_size: group_min_size,
                    const_size: group_const_size,
                }) = self.group_info.get(&group)
                {
                    min_size = group_min_size;
                    const_size = group_const_size;
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
            Expr::BacktrackingControlVerb(_) => {
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
                let child_info_truth = self.visit(
                    true_branch,
                    min_pos_in_group + child_info_condition.min_size,
                )?;
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
            Expr::SubroutineCall(target_group) => {
                // Track this subroutine call
                // Only skip tracking if we're in unreachable code at the root level
                // Calls inside groups should always be tracked, even if the group is inside {0} at root,
                // because the group can be called as a subroutine from elsewhere
                if !self.inside_zero_rep || self.current_group != 0 {
                    self.subroutine_calls
                        .entry(self.current_group)
                        .or_insert_with(Vec::new)
                        .push(SubroutineCallInfo {
                            target_group,
                            min_pos: min_pos_in_group,
                        });
                }

                // Look up the target group's min_size if available (similar to backrefs)
                // This is important for accurate left recursion detection
                if let Some(&SizeInfo {
                    min_size: group_min_size,
                    const_size: group_const_size,
                }) = self.group_info.get(&target_group)
                {
                    min_size = group_min_size;
                    const_size = group_const_size;
                } else {
                    // If the group hasn't been seen yet (forward reference),
                    // use conservative defaults
                    min_size = 0;
                    const_size = false;
                    self.contains_forward_referenced_subroutines = true;
                }
                hard = true;
            }
            Expr::UnresolvedNamedSubroutineCall { ref name, ix } => {
                return Err(Error::CompileError(Box::new(
                    CompileError::SubroutineCallTargetNotFound(name.to_string(), ix),
                )));
            }
            Expr::BackrefWithRelativeRecursionLevel { .. } => {
                return Err(Error::CompileError(Box::new(
                    CompileError::FeatureNotYetSupported("Backref at recursion level".to_string()),
                )));
            }
        };

        Ok(Info {
            expr,
            children,
            capture_groups: CaptureGroupRange(start_group, self.group_ix),
            min_size,
            const_size,
            hard,
            min_pos_in_group,
        })
    }

    /// Check for left-recursive subroutine calls using depth-first search
    pub(super) fn check_left_recursion(&self, named_groups: &Map<String, usize>) -> Result<()> {
        // Build reverse mapping from group number to group name (if any)
        // so we can give friendly error messages when left recursion is detected
        let mut group_names: Map<usize, String> = Map::new();
        for (name, &group_num) in named_groups.iter() {
            group_names.insert(group_num, name.clone());
        }

        // Compute which groups are reachable from the root (group 0)
        let reachable_groups = self.compute_reachable_groups();

        // Check each reachable group for left recursion
        for &start_group in self.subroutine_calls.keys() {
            if !reachable_groups.contains(start_group) {
                // Skip unreachable groups
                continue;
            }

            let mut visited = BitSet::new();
            let mut recursion_stack = BitSet::new();
            if self.dfs_check_left_recursion(start_group, &mut visited, &mut recursion_stack)? {
                // Found left recursion
                let group_desc = if let Some(name) = group_names.get(&start_group) {
                    format!("group '{}' ({})", name, start_group)
                } else {
                    format!("group {}", start_group)
                };
                return Err(Error::CompileError(Box::new(
                    CompileError::LeftRecursiveSubroutineCall(group_desc),
                )));
            }
        }
        Ok(())
    }

    /// A group is reachable if it's executed from root (not inside {0}) or called from a reachable group
    fn compute_reachable_groups(&self) -> BitSet {
        let mut reachable = BitSet::new();
        let mut to_visit = Vec::new();

        // Start from root (group 0)
        // Group 0 is always reachable
        reachable.insert(0);
        to_visit.push(0);

        // Also mark groups that are directly executed from root (not inside {0})
        for group in self.root_groups.iter() {
            if !reachable.contains(group) {
                reachable.insert(group);
                to_visit.push(group);
            }
        }

        // Propagate reachability through subroutine calls
        while let Some(group) = to_visit.pop() {
            if let Some(calls) = self.subroutine_calls.get(&group) {
                for call_info in calls {
                    if !reachable.contains(call_info.target_group) {
                        reachable.insert(call_info.target_group);
                        to_visit.push(call_info.target_group);
                    }
                }
            }
        }

        reachable
    }

    /// Rebuild subroutine_calls map by walking the Info tree with correct group sizes
    /// This fixes issues with forward references where min_size was unknown during first pass
    pub(super) fn rebuild_subroutine_calls(
        &mut self,
        info: &Info<'a>,
        current_group: usize,
        inside_zero_rep: bool,
    ) {
        self.rebuild_subroutine_calls_impl(info, current_group, 0, inside_zero_rep);
    }

    fn rebuild_subroutine_calls_impl(
        &mut self,
        info: &Info<'a>,
        current_group: usize,
        min_pos_in_group: usize,
        inside_zero_rep: bool,
    ) {
        match info.expr {
            Expr::Group(_) => {
                let group = info.start_group();
                // Track if this group is executed from root (not inside {0})
                if current_group == 0 && !inside_zero_rep {
                    self.root_groups.insert(group);
                }
                // Recurse into the group with position reset to 0
                if !info.children.is_empty() {
                    self.rebuild_subroutine_calls_impl(
                        &info.children[0],
                        group,
                        0,
                        inside_zero_rep,
                    );
                }
            }
            Expr::Concat(ref _v) => {
                let mut pos = min_pos_in_group;
                for child in info.children.iter() {
                    self.rebuild_subroutine_calls_impl(child, current_group, pos, inside_zero_rep);

                    // For SubroutineCalls, use the actual group's min_size, not the Info's min_size
                    // (which might be 0 due to forward references)
                    let child_min_size = if let Expr::SubroutineCall(target_group) = child.expr {
                        self.group_info
                            .get(target_group)
                            .map(|si| si.min_size)
                            .unwrap_or(child.min_size)
                    } else {
                        child.min_size
                    };

                    pos += child_min_size;
                }
            }
            Expr::Alt(_) => {
                // All alternatives start at the same position
                for child in &info.children {
                    self.rebuild_subroutine_calls_impl(
                        child,
                        current_group,
                        min_pos_in_group,
                        inside_zero_rep,
                    );
                }
            }
            Expr::Repeat { hi, .. } => {
                let new_inside_zero_rep = inside_zero_rep || *hi == 0;
                if !info.children.is_empty() {
                    self.rebuild_subroutine_calls_impl(
                        &info.children[0],
                        current_group,
                        min_pos_in_group,
                        new_inside_zero_rep,
                    );
                }
            }
            Expr::SubroutineCall(target_group) => {
                // Track this call with the correct position
                // Always track calls inside groups (they can be reached via subroutine calls)
                // Only skip calls at root level that are inside {0}
                if !inside_zero_rep || current_group != 0 {
                    self.subroutine_calls
                        .entry(current_group)
                        .or_insert_with(Vec::new)
                        .push(SubroutineCallInfo {
                            target_group: *target_group,
                            min_pos: min_pos_in_group,
                        });
                }
            }
            Expr::LookAround(_, _) | Expr::AtomicGroup(_) => {
                if !info.children.is_empty() {
                    self.rebuild_subroutine_calls_impl(
                        &info.children[0],
                        current_group,
                        min_pos_in_group,
                        inside_zero_rep,
                    );
                }
            }
            Expr::Conditional { .. } => {
                // Conditional has 3 children: condition, true_branch, false_branch
                if info.children.len() >= 3 {
                    self.rebuild_subroutine_calls_impl(
                        &info.children[0],
                        current_group,
                        min_pos_in_group,
                        inside_zero_rep,
                    );
                    let cond_size = info.children[0].min_size;
                    self.rebuild_subroutine_calls_impl(
                        &info.children[1],
                        current_group,
                        min_pos_in_group + cond_size,
                        inside_zero_rep,
                    );
                    self.rebuild_subroutine_calls_impl(
                        &info.children[2],
                        current_group,
                        min_pos_in_group,
                        inside_zero_rep,
                    );
                }
            }
            _ => {
                // For other expressions, just recurse into children
                for child in &info.children {
                    self.rebuild_subroutine_calls_impl(
                        child,
                        current_group,
                        min_pos_in_group,
                        inside_zero_rep,
                    );
                }
            }
        }
    }

    /// Depth-first search to detect left recursion
    /// Returns true if left recursion is detected
    fn dfs_check_left_recursion(
        &self,
        group: usize,
        visited: &mut BitSet,
        recursion_stack: &mut BitSet,
    ) -> Result<bool> {
        if recursion_stack.contains(group) {
            // We found a cycle. Since we only follow calls at position 0 (see below),
            // reaching a group already in the recursion stack means we have a left-recursive cycle.
            return Ok(true);
        }

        if visited.contains(group) {
            return Ok(false);
        }

        visited.insert(group);
        recursion_stack.insert(group);

        // Check all subroutine calls from this group
        if let Some(calls) = self.subroutine_calls.get(&group) {
            for call_info in calls {
                // Only consider calls at position 0 (potential left recursion)
                if call_info.min_pos == 0 {
                    if self.dfs_check_left_recursion(
                        call_info.target_group,
                        visited,
                        recursion_stack,
                    )? {
                        return Ok(true);
                    }
                }
            }
        }

        recursion_stack.remove(group);
        Ok(false)
    }
}

fn literal_const_size(_: &str, _: bool) -> bool {
    // Right now, regex doesn't do sophisticated case folding,
    // test below will fail when that changes, then we need to
    // do something fancier here.
    true
}
