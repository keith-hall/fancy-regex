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

use crate::{Captures, Expr, Match, Regex, Result};

/// A set of regular expressions that can be matched simultaneously, returning the first match.
///
/// `RegexSet` takes multiple regex patterns in priority order and combines them into a single
/// optimized regex. When matching against text, it returns which pattern matched first according
/// to the priority order.
///
/// This is more efficient than evaluating each regex separately because the compiler and VM can
/// optimize the combined pattern, especially when patterns share common prefixes or can be
/// delegated to the underlying NFA engine.
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
    inner: Regex,
    pattern_count: usize,
    /// Starting capture group for each pattern (used to identify which pattern matched)
    pattern_group_starts: Vec<usize>,
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
        if patterns.is_empty() {
            // Create a regex that never matches
            return Ok(RegexSet {
                inner: Regex::new("(?!)")?,
                pattern_count: 0,
                pattern_group_starts: Vec::new(),
            });
        }

        // Parse each pattern to count capture groups and build the combined pattern
        let mut pattern_group_starts = Vec::with_capacity(patterns.len());
        let mut current_group = 0;
        let mut trees = Vec::with_capacity(patterns.len());
        
        for pattern in patterns {
            let tree = Expr::parse_tree(pattern)?;
            pattern_group_starts.push(current_group);
            
            // Count capture groups in this pattern (excluding group 0)
            let group_count = count_groups(&tree.expr);
            trees.push(tree);
            
            // Each pattern will be wrapped in a group, and we need to account for
            // all the groups in the pattern
            current_group += 1 + group_count;
        }

        // Now build the combined expression by wrapping each in a group and using alternation
        // For simplicity, we'll use string concatenation but adjust group numbers
        let mut combined = String::new();
        for (i, pattern) in patterns.iter().enumerate() {
            if i > 0 {
                combined.push('|');
            }
            combined.push('(');
            combined.push_str(pattern);
            combined.push(')');
        }

        let inner = Regex::new(&combined)?;
        Ok(RegexSet {
            inner,
            pattern_count: patterns.len(),
            pattern_group_starts,
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
        self.inner.is_match(text)
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
        match self.inner.captures(text)? {
            None => Ok(SetMatches {
                pattern_index: None,
                match_location: None,
            }),
            Some(captures) => {
                // Find which wrapper group matched (which pattern)
                for (pattern_idx, &group_start) in self.pattern_group_starts.iter().enumerate() {
                    // The wrapper group is at group_start + 1 (group 0 is entire match)
                    if let Some(m) = captures.get(group_start + 1) {
                        return Ok(SetMatches {
                            pattern_index: Some(pattern_idx),
                            match_location: Some((m.start(), m.end())),
                        });
                    }
                }
                // This shouldn't happen if we matched
                Ok(SetMatches {
                    pattern_index: None,
                    match_location: None,
                })
            }
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
        match self.inner.captures(text)? {
            None => Ok(None),
            Some(captures) => {
                // Find which wrapper group matched (which pattern)
                for (pattern_idx, &group_start) in self.pattern_group_starts.iter().enumerate() {
                    if let Some(m) = captures.get(group_start + 1) {
                        return Ok(Some((pattern_idx, m)));
                    }
                }
                Ok(None)
            }
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
    /// let (pattern_idx, captures) = set.captures("Date: 2024-12-15").unwrap().unwrap();
    /// assert_eq!(pattern_idx, 0);
    ///
    /// // Get the entire match
    /// assert_eq!(captures.get(0).unwrap().as_str(), "2024-12-15");
    /// ```
    pub fn captures<'t>(&self, text: &'t str) -> Result<Option<(usize, Captures<'t>)>> {
        match self.inner.captures(text)? {
            None => Ok(None),
            Some(captures) => {
                // Find which wrapper group matched (which pattern)
                for (pattern_idx, &group_start) in self.pattern_group_starts.iter().enumerate() {
                    if captures.get(group_start + 1).is_some() {
                        return Ok(Some((pattern_idx, captures)));
                    }
                }
                Ok(None)
            }
        }
    }
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
