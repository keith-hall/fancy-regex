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

//! RegexSet for matching multiple patterns efficiently.

use alloc::string::String;
use alloc::vec::Vec;

use crate::parse::ExprTree;
use crate::{Captures, Expr, Match, Regex, Result};

/// A set of regular expressions that can be matched simultaneously, returning the first match.
///
/// `RegexSet` takes multiple regex patterns in priority order and combines them into optimized
/// regex expressions. Patterns are analyzed and grouped into "easy" (can be delegated to NFA)
/// and "hard" (requiring backtracking VM) sets for better performance.
///
/// The easy patterns are evaluated first. If an easy pattern matches at the start position and
/// has higher priority than all hard patterns, the result is returned without evaluating hard
/// patterns.
///
/// # Example
///
/// ```rust
/// use fancy_regex::RegexSet;
///
/// let set = RegexSet::new(&[
///     r"\d{4}-\d{2}-\d{2}",  // Date pattern (priority 0)
///     r"\w+@\w+\.\w+",        // Email pattern (priority 1)
///     r"\d+",                 // Number pattern (priority 2)
/// ]).unwrap();
///
/// let result = set.matches("Contact: user@example.com").unwrap();
/// assert!(result.matched(1)); // Email pattern matched
/// ```
#[derive(Clone, Debug)]
pub struct RegexSet {
    /// Combined regex for easy patterns (can be delegated to NFA)
    easy_regex: Option<Regex>,
    /// Combined regex for hard patterns (requires VM)
    hard_regex: Option<Regex>,
    /// Total number of patterns
    pattern_count: usize,
    /// Information about each pattern
    pattern_info: Vec<PatternInfo>,
}

#[derive(Clone, Debug)]
struct PatternInfo {
    /// Original pattern index (0-based)
    pattern_index: usize,
    /// Whether this pattern is "hard" (requires backtracking VM)
    is_hard: bool,
    /// Starting capture group in the combined easy or hard regex
    group_start: usize,
    /// Lowest index of any hard pattern (for short-circuit check)
    first_hard_pattern_idx: Option<usize>,
}

/// Result of a `RegexSet` match operation.
///
/// Contains information about which pattern matched and the match location.
#[derive(Debug)]
pub struct SetMatches {
    pattern_index: Option<usize>,
    match_location: Option<(usize, usize)>,
}

impl RegexSet {
    /// Creates a new `RegexSet` from a slice of pattern strings.
    ///
    /// The patterns are evaluated in priority order - if multiple patterns could match at the
    /// same location, the one with the lowest index (earliest in the slice) is returned.
    ///
    /// # Errors
    ///
    /// Returns an error if any of the patterns are invalid regex patterns.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[r"\d+", r"\w+"]).unwrap();
    /// ```
    pub fn new(patterns: &[&str]) -> Result<RegexSet> {
        Self::new_impl(patterns, &crate::RegexOptions::default())
    }
    
    fn new_impl(patterns: &[&str], options: &crate::RegexOptions) -> Result<RegexSet> {
        if patterns.is_empty() {
            return Ok(RegexSet {
                easy_regex: None,
                hard_regex: None,
                pattern_count: 0,
                pattern_info: Vec::new(),
            });
        }

        // First pass: analyze each pattern to determine if it's hard or easy
        let mut analyzed_patterns = Vec::with_capacity(patterns.len());
        for (pattern_idx, pattern) in patterns.iter().enumerate() {
            let tree = Expr::parse_tree_with_flags(pattern, options.compute_flags())?;
            
            // Analyze to determine if hard (clone the tree to avoid borrowing issues)
            use crate::analyze::analyze;
            let tree_for_analysis = tree.clone();
            let info = analyze(&tree_for_analysis, false)?;
            
            analyzed_patterns.push((pattern_idx, *pattern, tree, info.hard));
        }

        // Find first hard pattern index
        let first_hard_pattern_idx = analyzed_patterns
            .iter()
            .find(|(_, _, _, is_hard)| *is_hard)
            .map(|(idx, _, _, _)| *idx);

        // Separate into easy and hard patterns, maintaining their order
        let mut easy_patterns = Vec::new();
        let mut hard_patterns = Vec::new();
        
        for (pattern_idx, pattern, tree, is_hard) in analyzed_patterns {
            if is_hard {
                hard_patterns.push((pattern_idx, pattern, tree));
            } else {
                easy_patterns.push((pattern_idx, pattern, tree));
            }
        }

        // Build pattern_info vec
        let mut pattern_info = Vec::with_capacity(patterns.len());

        // Build combined regex for easy patterns
        let easy_regex = if !easy_patterns.is_empty() {
            Some(build_combined_regex(&easy_patterns, options, &mut pattern_info, false, first_hard_pattern_idx)?)
        } else {
            None
        };

        // Build combined regex for hard patterns
        let hard_regex = if !hard_patterns.is_empty() {
            Some(build_combined_regex(&hard_patterns, options, &mut pattern_info, true, first_hard_pattern_idx)?)
        } else {
            None
        };

        // Sort pattern_info by original pattern index to maintain priority order
        pattern_info.sort_by_key(|pi| pi.pattern_index);

        Ok(RegexSet {
            easy_regex,
            hard_regex,
            pattern_count: patterns.len(),
            pattern_info,
        })
    }

    /// Returns the number of patterns in the set.
    pub fn len(&self) -> usize {
        self.pattern_count
    }

    /// Returns true if the set contains no patterns.
    pub fn is_empty(&self) -> bool {
        self.pattern_count == 0
    }

    /// Checks if any pattern in the set matches the text.
    ///
    /// Returns `true` if at least one pattern matches.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();
    /// assert!(set.is_match("abc").unwrap());
    /// assert!(set.is_match("123").unwrap());
    /// assert!(!set.is_match("!!!").unwrap());
    /// ```
    pub fn is_match(&self, text: &str) -> Result<bool> {
        Ok(self.matches(text)?.pattern().is_some())
    }

    /// Returns information about which pattern matched the text.
    ///
    /// If multiple patterns could match, returns the one with the lowest index (highest priority).
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[
    ///     r"\d+",      // Pattern 0
    ///     r"[0-9]+",   // Pattern 1 (would also match digits, but lower priority)
    /// ]).unwrap();
    ///
    /// let result = set.matches("123").unwrap();
    /// assert!(result.matched(0)); // Pattern 0 matched first
    /// assert!(!result.matched(1)); // Pattern 1 didn't match (pattern 0 took precedence)
    /// ```
    pub fn matches(&self, text: &str) -> Result<SetMatches> {
        // Try easy patterns first
        if let Some(ref easy_regex) = self.easy_regex {
            if let Some(captures) = easy_regex.captures(text)? {
                // Find which pattern matched in easy regex
                for info in &self.pattern_info {
                    if !info.is_hard {
                        if let Some(m) = captures.get(info.group_start + 1) {
                            // Check if we can short-circuit: the match is at position 0
                            // and there are no hard patterns with higher priority
                            let can_short_circuit = m.start() == 0
                                && (info.first_hard_pattern_idx.is_none()
                                    || info.pattern_index < info.first_hard_pattern_idx.unwrap());

                            if can_short_circuit {
                                // No need to check hard patterns
                                return Ok(SetMatches {
                                    pattern_index: Some(info.pattern_index),
                                    match_location: Some((m.start(), m.end())),
                                });
                            }
                            
                            // Can't short-circuit, will need to check hard patterns too
                            break;
                        }
                    }
                }
            }
        }

        // Need to check both easy and hard patterns to find the highest priority match
        let mut best_match: Option<(usize, usize, usize)> = None; // (pattern_idx, start, end)

        // Check easy patterns
        if let Some(ref easy_regex) = self.easy_regex {
            if let Some(captures) = easy_regex.captures(text)? {
                for info in &self.pattern_info {
                    if !info.is_hard {
                        if let Some(m) = captures.get(info.group_start + 1) {
                            best_match = Some((info.pattern_index, m.start(), m.end()));
                            break;
                        }
                    }
                }
            }
        }

        // Check hard patterns
        if let Some(ref hard_regex) = self.hard_regex {
            if let Some(captures) = hard_regex.captures(text)? {
                for info in &self.pattern_info {
                    if info.is_hard {
                        if let Some(m) = captures.get(info.group_start + 1) {
                            // Check if this hard match beats the easy match
                            if let Some((best_idx, best_start, _)) = best_match {
                                // Hard pattern wins if:
                                // 1. It matches earlier in the text, OR
                                // 2. It matches at the same position but has lower index (higher priority)
                                if m.start() < best_start
                                    || (m.start() == best_start && info.pattern_index < best_idx)
                                {
                                    best_match = Some((info.pattern_index, m.start(), m.end()));
                                }
                            } else {
                                best_match = Some((info.pattern_index, m.start(), m.end()));
                            }
                            break;
                        }
                    }
                }
            }
        }

        if let Some((pattern_idx, start, end)) = best_match {
            Ok(SetMatches {
                pattern_index: Some(pattern_idx),
                match_location: Some((start, end)),
            })
        } else {
            Ok(SetMatches {
                pattern_index: None,
                match_location: None,
            })
        }
    }

    /// Returns the first match in the text, including which pattern matched.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[r"\d{4}", r"\d+"]).unwrap();
    /// let (pattern_idx, mat) = set.find("The year 2024 has 365 days").unwrap().unwrap();
    ///
    /// assert_eq!(pattern_idx, 0); // First pattern matched
    /// assert_eq!(mat.as_str(), "2024");
    /// ```
    pub fn find<'t>(&self, text: &'t str) -> Result<Option<(usize, Match<'t>)>> {
        let result = self.matches(text)?;
        if let (Some(pattern_idx), Some((start, end))) =
            (result.pattern_index, result.match_location)
        {
            Ok(Some((pattern_idx, Match::new(text, start, end))))
        } else {
            Ok(None)
        }
    }

    /// Returns the captures for the first matching pattern.
    ///
    /// This is useful when patterns contain their own capture groups.
    ///
    /// Note: The capture groups returned include the wrapper group for identifying which pattern
    /// matched. You can access the pattern's own capture groups starting from the appropriate offset.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[
    ///     r"(\d{4})-(\d{2})-(\d{2})", // Date with capture groups
    ///     r"\w+",
    /// ]).unwrap();
    ///
    /// // Use text where date appears first
    /// let (pattern_idx, captures) = set.captures("2024-12-15 is the date").unwrap().unwrap();
    /// assert_eq!(pattern_idx, 0);
    ///
    /// // Get the entire match
    /// assert_eq!(captures.get(0).unwrap().as_str(), "2024-12-15");
    /// ```
    pub fn captures<'t>(&self, text: &'t str) -> Result<Option<(usize, Captures<'t>)>> {
        // Similar to matches(), but returns full captures
        let matches_result = self.matches(text)?;
        
        if matches_result.pattern_index.is_none() {
            return Ok(None);
        }
        
        let pattern_idx = matches_result.pattern_index.unwrap();
        let info = &self.pattern_info[pattern_idx];
        
        // Get captures from the appropriate regex
        if info.is_hard {
            if let Some(ref hard_regex) = self.hard_regex {
                if let Some(captures) = hard_regex.captures(text)? {
                    return Ok(Some((pattern_idx, captures)));
                }
            }
        } else {
            if let Some(ref easy_regex) = self.easy_regex {
                if let Some(captures) = easy_regex.captures(text)? {
                    return Ok(Some((pattern_idx, captures)));
                }
            }
        }
        
        Ok(None)
    }
}

/// Helper function to build a combined regex from a set of patterns
fn build_combined_regex(
    patterns: &[(usize, &str, ExprTree)],
    options: &crate::RegexOptions,
    pattern_info: &mut Vec<PatternInfo>,
    is_hard: bool,
    first_hard_pattern_idx: Option<usize>,
) -> Result<Regex> {
    use alloc::sync::Arc;
    use crate::analyze::{analyze, can_compile_as_anchored};
    use crate::compile::compile;
    use crate::optimize::optimize;
    use crate::RegexImpl;

    let mut current_group = 0;
    let mut adjusted_exprs = Vec::with_capacity(patterns.len());
    let mut combined_backrefs = bit_set::BitSet::new();
    let mut combined_named_groups = crate::parse::NamedGroups::default();

    for (pattern_idx, _pattern, tree) in patterns {
        let mut tree_clone = tree.clone();
        
        // Count capture groups in this pattern (excluding group 0)
        let group_count = count_groups(&tree_clone.expr);
        
        // Record pattern info
        pattern_info.push(PatternInfo {
            pattern_index: *pattern_idx,
            is_hard,
            group_start: current_group,
            first_hard_pattern_idx,
        });
        
        // Adjust backreferences in the pattern to account for the wrapper group
        // and previous patterns' groups
        adjust_group_numbers(&mut tree_clone.expr, current_group + 1);
        
        // Merge backrefs from this pattern (after adjusting)
        for backref in tree_clone.backrefs.iter() {
            combined_backrefs.insert(backref + current_group + 1);
        }
        
        // Merge named groups (adjusting their indices)
        for (name, &idx) in tree_clone.named_groups.iter() {
            combined_named_groups.insert(name.clone(), idx + current_group + 1);
        }
        
        adjusted_exprs.push(tree_clone.expr);
        
        // Each pattern will be wrapped in a group, and we need to account for
        // all the groups in the pattern
        current_group += 1 + group_count;
    }

    // Build the combined expression as an alternation of wrapped patterns
    let mut alt_children = Vec::with_capacity(patterns.len());
    for expr in adjusted_exprs {
        // Wrap each pattern in a capture group
        alt_children.push(Expr::Group(Box::new(expr)));
    }
    
    let combined_expr = if alt_children.len() == 1 {
        alt_children.into_iter().next().unwrap()
    } else {
        Expr::Alt(alt_children)
    };
    
    // Create an ExprTree for the combined expression
    let combined_tree = ExprTree {
        expr: combined_expr,
        backrefs: combined_backrefs,
        named_groups: combined_named_groups,
        contains_subroutines: false,
        self_recursive: false,
    };
    
    // Now compile using the same logic as Regex::new_options
    let mut tree = combined_tree;
    let requires_capture_group_fixup = optimize(&mut tree);
    let info = analyze(&tree, requires_capture_group_fixup)?;
    
    let inner = if !info.hard {
        // Easy case - delegate to regex crate
        let mut re_cooked = String::new();
        tree.expr.to_str(&mut re_cooked, 0);
        let inner_re = crate::compile::compile_inner(&re_cooked, options)?;
        Regex {
            inner: RegexImpl::Wrap {
                inner: inner_re,
                options: options.clone(),
                explicit_capture_group_0: requires_capture_group_fixup,
                debug_pattern: re_cooked,
            },
            named_groups: Arc::new(tree.named_groups),
        }
    } else {
        // Hard case - use VM
        let prog = compile(&info, can_compile_as_anchored(&tree.expr))?;
        Regex {
            inner: RegexImpl::Fancy {
                prog: Arc::new(prog),
                n_groups: info.end_group(),
                options: options.clone(),
            },
            named_groups: Arc::new(tree.named_groups),
        }
    };
    
    Ok(inner)
}

/// Count the number of capture groups in an expression (excluding group 0)
fn count_groups(expr: &Expr) -> usize {
    match expr {
        Expr::Group(_) => {
            // This is a capture group, count it and any nested groups
            1 + count_groups_in_children(expr)
        }
        _ => count_groups_in_children(expr),
    }
}

fn count_groups_in_children(expr: &Expr) -> usize {
    match expr {
        Expr::Group(child)
        | Expr::LookAround(child, _)
        | Expr::AtomicGroup(child)
        | Expr::Repeat { child, .. } => count_groups(child),
        Expr::Concat(children) | Expr::Alt(children) => {
            children.iter().map(|e| count_groups(e)).sum()
        }
        Expr::Conditional {
            condition,
            true_branch,
            false_branch,
        } => count_groups(condition) + count_groups(true_branch) + count_groups(false_branch),
        _ => 0,
    }
}

/// Adjust backreference and group numbers in an expression tree
/// to account for wrapper groups and previous patterns.
/// `offset` is the number of groups to add to backreferences.
fn adjust_group_numbers(expr: &mut Expr, offset: usize) {
    match expr {
        Expr::Backref { group, .. } => {
            *group += offset;
        }
        Expr::BackrefWithRelativeRecursionLevel { group, .. } => {
            *group += offset;
        }
        Expr::BackrefExistsCondition(group) => {
            *group += offset;
        }
        Expr::SubroutineCall(group) => {
            *group += offset;
        }
        Expr::Group(child)
        | Expr::LookAround(child, _)
        | Expr::AtomicGroup(child)
        | Expr::Repeat { child, .. } => {
            adjust_group_numbers(child, offset);
        }
        Expr::Concat(children) | Expr::Alt(children) => {
            for child in children {
                adjust_group_numbers(child, offset);
            }
        }
        Expr::Conditional {
            condition,
            true_branch,
            false_branch,
        } => {
            adjust_group_numbers(condition, offset);
            adjust_group_numbers(true_branch, offset);
            adjust_group_numbers(false_branch, offset);
        }
        _ => {}
    }
}

impl SetMatches {
    /// Returns whether the pattern at the given index matched.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fancy_regex::RegexSet;
    ///
    /// let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();
    /// let result = set.matches("123").unwrap();
    ///
    /// assert!(result.matched(0));
    /// assert!(!result.matched(1));
    /// ```
    pub fn matched(&self, pattern_index: usize) -> bool {
        self.pattern_index == Some(pattern_index)
    }

    /// Returns the index of the pattern that matched, if any.
    pub fn pattern(&self) -> Option<usize> {
        self.pattern_index
    }

    /// Returns the byte range of the match, if any.
    pub fn match_range(&self) -> Option<(usize, usize)> {
        self.match_location
    }
}
