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

//! RegexSet for matching multiple patterns with priority ordering
//!
//! This module provides a `RegexSet` type that can match multiple regular expression patterns
//! against a text and return the first match based on position and priority order.
//!
//! # Example
//!
//! ```
//! use fancy_regex::RegexSet;
//!
//! let set = RegexSet::new(&["foo", "bar", "baz"]).unwrap();
//! let text = "hello bar world";
//! let result = set.find(text).unwrap();
//!
//! assert!(result.is_some());
//! let m = result.unwrap();
//! assert_eq!(m.pattern(), 1); // pattern "bar" matched
//! assert_eq!(m.as_str(), "bar");
//! ```
//!
//! # Priority-based Matching
//!
//! When multiple patterns match at different positions, the match with the lowest starting
//! position is returned. When multiple patterns match at the same position, the pattern with
//! the lowest index (highest priority) is returned.
//!
//! ```
//! use fancy_regex::RegexSet;
//!
//! let set = RegexSet::new(&["foo", "f"]).unwrap();
//! let text = "foo";
//! let result = set.find(text).unwrap();
//!
//! // Both patterns match at position 0, but "foo" has higher priority (lower index)
//! assert_eq!(result.unwrap().pattern(), 0);
//! ```
//!
//! # Mixed Easy and Hard Patterns
//!
//! `RegexSet` automatically classifies patterns as "easy" (can be delegated to the fast
//! `regex-automata` engine) or "hard" (require backtracking VM execution). Easy patterns
//! are matched efficiently using `regex-automata`, while hard patterns with features like
//! backreferences and lookaround are executed using the VM.
//!
//! With the `std` feature enabled, hard patterns are searched in parallel using multiple
//! threads for better performance.
//!
//! ```
//! use fancy_regex::RegexSet;
//!
//! // Mix of easy and hard patterns
//! let set = RegexSet::new(&[
//!     r"simple",           // easy pattern
//!     r"(\w+)\1",          // hard pattern (backreference)
//!     r"easy",             // easy pattern
//! ]).unwrap();
//!
//! let text = "foofoo easy simple";
//! let result = set.find(text).unwrap().unwrap();
//! assert_eq!(result.as_str(), "foofoo");
//! assert_eq!(result.pattern(), 1);
//! ```

use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use regex_automata::meta::Regex as RaRegex;
use regex_automata::Input as RaInput;

use crate::analyze::analyze;
use crate::compile::{compile, compile_inner};
use crate::optimize::optimize;
use crate::vm::{self, Prog};
use crate::{Expr, Match, RegexOptions, Result};

/// A builder for constructing a RegexSet
///
/// # Example
///
/// ```
/// use fancy_regex::RegexSetBuilder;
///
/// let set = RegexSetBuilder::new(&["foo", "bar"])
///     .build()
///     .unwrap();
///
/// let result = set.find("foobar").unwrap().unwrap();
/// assert_eq!(result.as_str(), "foo");
/// ```
///
/// With the `std` feature, you can configure the maximum number of threads:
///
/// ```
/// # #[cfg(feature = "std")]
/// # {
/// use fancy_regex::RegexSetBuilder;
///
/// let set = RegexSetBuilder::new(&[r"(\w+)\1", r"easy"])
///     .max_threads(8)
///     .build()
///     .unwrap();
/// # }
/// ```
#[derive(Debug)]
pub struct RegexSetBuilder {
    patterns: Vec<String>,
    options: RegexOptions,
    #[cfg(feature = "std")]
    max_threads: usize,
}

impl RegexSetBuilder {
    /// Create a new RegexSet builder with the given patterns
    pub fn new<I, S>(patterns: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        RegexSetBuilder {
            patterns: patterns.into_iter().map(|s| s.as_ref().to_string()).collect(),
            options: RegexOptions::default(),
            #[cfg(feature = "std")]
            max_threads: 4,
        }
    }

    /// Set the maximum number of threads to use for searching hard patterns
    /// (only available with the `std` feature)
    #[cfg(feature = "std")]
    pub fn max_threads(&mut self, max_threads: usize) -> &mut Self {
        self.max_threads = max_threads;
        self
    }

    /// Build the RegexSet
    pub fn build(&self) -> Result<RegexSet> {
        RegexSet::new_with_options(
            &self.patterns,
            self.options.clone(),
            #[cfg(feature = "std")]
            self.max_threads,
        )
    }
}

/// A set of compiled regular expressions for matching with priority ordering
///
/// `RegexSet` matches multiple patterns against a text and returns the first match
/// based on position and priority order. Patterns are specified in priority order,
/// with the first pattern having the highest priority.
///
/// # Matching Behavior
///
/// When searching for matches:
/// 1. The match with the lowest starting position in the haystack wins
/// 2. If multiple patterns match at the same position, the pattern with the
///    lowest index (highest priority) wins
/// 3. Easy patterns (without backreferences/lookaround) are matched using the
///    fast `regex-automata` engine
/// 4. Hard patterns (with backreferences/lookaround) are matched using the VM
///
/// # Threading
///
/// With the `std` feature enabled, hard patterns are searched in parallel using
/// multiple threads for better performance. The number of threads can be configured
/// using `RegexSetBuilder::max_threads()`. Without the `std` feature, patterns are
/// searched sequentially.
///
/// # Example
///
/// ```
/// use fancy_regex::RegexSet;
///
/// let set = RegexSet::new(&["foo", "bar", "baz"]).unwrap();
///
/// let result = set.find("hello bar world").unwrap().unwrap();
/// assert_eq!(result.pattern(), 1);
/// assert_eq!(result.as_str(), "bar");
/// assert_eq!(result.start(), 6);
/// assert_eq!(result.end(), 9);
/// ```
///
/// # Priority Example
///
/// ```
/// use fancy_regex::RegexSet;
///
/// let set = RegexSet::new(&["bar", "foo"]).unwrap();
/// let text = "foobar";
///
/// // "foo" appears first in the text, so it matches even though "bar" has higher priority
/// let result = set.find(text).unwrap().unwrap();
/// assert_eq!(result.as_str(), "foo");
/// assert_eq!(result.pattern(), 1);
/// ```
#[derive(Debug)]
pub struct RegexSet {
    /// Patterns indexed by their priority (index 0 is highest priority)
    patterns: Vec<String>,
    /// Easy patterns that can be delegated to regex-automata
    easy_patterns: Vec<EasyPattern>,
    /// Hard patterns that need VM execution
    hard_patterns: Vec<HardPattern>,
    #[cfg(feature = "std")]
    /// Maximum number of threads for parallel hard pattern searching
    max_threads: usize,
}

#[derive(Debug)]
struct EasyPattern {
    /// The index in the original patterns array
    index: usize,
    /// The compiled regex
    regex: RaRegex,
    /// Options used for compilation
    options: RegexOptions,
    /// Whether this pattern requires extracting capture group 1 for the match boundaries
    explicit_capture_group_0: bool,
}

#[derive(Debug)]
struct HardPattern {
    /// The index in the original patterns array
    index: usize,
    /// The compiled VM program
    prog: Arc<Prog>,
    /// Number of capture groups
    n_groups: usize,
    /// Options used for compilation
    options: RegexOptions,
}

/// Result of a RegexSet match operation
///
/// Contains information about which pattern matched and the match details.
///
/// # Example
///
/// ```
/// use fancy_regex::RegexSet;
///
/// let set = RegexSet::new(&["foo", "bar"]).unwrap();
/// let result = set.find("hello bar").unwrap().unwrap();
///
/// assert_eq!(result.pattern(), 1);
/// assert_eq!(result.as_str(), "bar");
/// assert_eq!(result.start(), 6);
/// assert_eq!(result.end(), 9);
/// ```
#[derive(Debug, Clone)]
pub struct SetMatch<'t> {
    /// The index of the matching pattern
    pattern: usize,
    /// The match details
    match_: Match<'t>,
}

impl<'t> SetMatch<'t> {
    /// Returns the index of the pattern that matched
    pub fn pattern(&self) -> usize {
        self.pattern
    }

    /// Returns the match details
    pub fn match_(&self) -> &Match<'t> {
        &self.match_
    }

    /// Returns the starting byte offset of the match
    pub fn start(&self) -> usize {
        self.match_.start()
    }

    /// Returns the ending byte offset of the match
    pub fn end(&self) -> usize {
        self.match_.end()
    }

    /// Returns the matched text
    pub fn as_str(&self) -> &'t str {
        self.match_.as_str()
    }
}

impl RegexSet {
    /// Create a new RegexSet from an iterator of patterns
    pub fn new<I, S>(patterns: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let patterns: Vec<String> = patterns.into_iter().map(|s| s.as_ref().to_string()).collect();
        Self::new_with_options(
            &patterns,
            RegexOptions::default(),
            #[cfg(feature = "std")]
            4,
        )
    }

    fn new_with_options(
        patterns: &[String],
        base_options: RegexOptions,
        #[cfg(feature = "std")] max_threads: usize,
    ) -> Result<Self> {
        let mut easy_patterns = Vec::new();
        let mut hard_patterns = Vec::new();

        // Parse and analyze each pattern
        for (index, pattern) in patterns.iter().enumerate() {
            let options = RegexOptions {
                pattern: pattern.clone(),
                ..base_options.clone()
            };

            let mut tree = Expr::parse_tree_with_flags(pattern, options.compute_flags())?;
            let requires_capture_group_fixup = optimize(&mut tree);
            let info = analyze(&tree, requires_capture_group_fixup)?;

            if !info.hard {
                // Easy pattern - can be delegated to regex-automata
                let mut re_cooked = String::new();
                tree.expr.to_str(&mut re_cooked, 0);
                let regex = compile_inner(&re_cooked, &options)?;

                easy_patterns.push(EasyPattern {
                    index,
                    regex,
                    options,
                    explicit_capture_group_0: requires_capture_group_fixup,
                });
            } else {
                // Hard pattern - needs VM execution
                let prog = compile(&info, false)?;
                hard_patterns.push(HardPattern {
                    index,
                    prog: Arc::new(prog),
                    n_groups: info.end_group(),
                    options,
                });
            }
        }

        Ok(RegexSet {
            patterns: patterns.to_vec(),
            easy_patterns,
            hard_patterns,
            #[cfg(feature = "std")]
            max_threads,
        })
    }

    /// Returns the patterns in the set
    pub fn patterns(&self) -> &[String] {
        &self.patterns
    }

    /// Returns the number of patterns in the set
    pub fn len(&self) -> usize {
        self.patterns.len()
    }

    /// Returns true if the set has no patterns
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    /// Find the first match among all patterns in the haystack
    ///
    /// Returns the match with the lowest start position. If multiple patterns match
    /// at the same position, returns the one with the highest priority (lowest index).
    pub fn find<'t>(&self, text: &'t str) -> Result<Option<SetMatch<'t>>> {
        self.find_from_pos(text, 0)
    }

    /// Find the first match starting from the given position
    pub fn find_from_pos<'t>(&self, text: &'t str, pos: usize) -> Result<Option<SetMatch<'t>>> {
        // Find the best easy match
        let easy_match = self.find_easy_match(text, pos);

        // Find the best hard match
        let hard_match = self.find_hard_match(text, pos)?;

        // Return the better of the two matches
        Ok(Self::choose_best_match(easy_match, hard_match))
    }

    fn find_easy_match<'t>(&self, text: &'t str, pos: usize) -> Option<SetMatch<'t>> {
        let mut best_match: Option<SetMatch<'t>> = None;

        for easy_pattern in &self.easy_patterns {
            let current_match = if !easy_pattern.explicit_capture_group_0 {
                // Simple case: use search() directly
                easy_pattern
                    .regex
                    .search(&RaInput::new(text).span(pos..text.len()))
                    .map(|m| SetMatch {
                        pattern: easy_pattern.index,
                        match_: Match::new(text, m.start(), m.end()),
                    })
            } else {
                // Pattern has trailing lookahead: extract capture group 1 for match boundaries
                let mut locations = easy_pattern.regex.create_captures();
                easy_pattern
                    .regex
                    .captures(RaInput::new(text).span(pos..text.len()), &mut locations);
                if locations.is_match() {
                    let group1 = locations.get_group(1).unwrap();
                    Some(SetMatch {
                        pattern: easy_pattern.index,
                        match_: Match::new(text, group1.start, group1.end),
                    })
                } else {
                    None
                }
            };

            if let Some(current_match) = current_match {
                best_match = match best_match {
                    None => Some(current_match),
                    Some(existing) => Some(Self::choose_best_of_two(existing, current_match)),
                };

                // Early termination optimization:
                // If we found a match at the starting position and it's from the highest priority
                // pattern we've checked so far, and all patterns with even higher priority have
                // been checked, we can stop searching.
                if let Some(ref m) = best_match {
                    if m.start() == pos {
                        // Match at start position - check if we can terminate early
                        let best_pattern_idx = m.pattern();
                        
                        // Check if all higher priority patterns (lower indices) have been checked
                        let all_higher_checked = (0..best_pattern_idx).all(|i| {
                            // Pattern i must either be in easy_patterns or hard_patterns and
                            // have been processed already
                            let in_easy = self.easy_patterns.iter()
                                .any(|ep| ep.index == i && ep.index <= easy_pattern.index);
                            let in_hard = self.hard_patterns.iter().any(|hp| hp.index == i);
                            in_easy || in_hard
                        });
                        
                        if all_higher_checked {
                            break;
                        }
                    }
                }
            }
        }

        best_match
    }

    #[cfg(feature = "std")]
    fn find_hard_match<'t>(&self, text: &'t str, pos: usize) -> Result<Option<SetMatch<'t>>> {
        use std::sync::{Arc, Mutex};
        use std::thread;

        if self.hard_patterns.is_empty() {
            return Ok(None);
        }

        // Use Arc<str> to share the text across threads without copying
        let text_arc: Arc<str> = Arc::from(text);
        // Store results as (start, pattern_index, end) to avoid lifetime issues with Match<'t>
        let best_match: Arc<Mutex<Option<(usize, usize, usize)>>> = Arc::new(Mutex::new(None));

        // Process hard patterns in chunks based on max_threads
        for chunk in self.hard_patterns.chunks(self.max_threads.max(1)) {
            let mut chunk_handles = Vec::new();

            for hard_pattern in chunk {
                let best_match = Arc::clone(&best_match);
                let text_arc = Arc::clone(&text_arc);
                let prog = Arc::clone(&hard_pattern.prog);
                let options = hard_pattern.options.clone();
                let index = hard_pattern.index;

                let handle = thread::spawn(move || -> Result<()> {
                    // Check if we can early terminate
                    {
                        let current_best = best_match.lock().unwrap();
                        if let Some((start, pattern, _)) = *current_best {
                            if start == pos && pattern < index {
                                // A higher priority pattern already matched at this position
                                return Ok(());
                            }
                        }
                    }

                    // Execute the VM to find a match
                    match vm::run(&prog, &text_arc, pos, 0, &options) {
                        Ok(Some(saves)) => {
                            let mut best = best_match.lock().unwrap();
                            let new_match = (saves[0], index, saves[1]);
                            *best = match *best {
                                None => Some(new_match),
                                Some((start_a, pattern_a, end_a)) => {
                                    let (start_b, pattern_b, end_b) = new_match;
                                    // Choose by position first (lower is better)
                                    match start_a.cmp(&start_b) {
                                        core::cmp::Ordering::Less => {
                                            Some((start_a, pattern_a, end_a))
                                        }
                                        core::cmp::Ordering::Greater => {
                                            Some((start_b, pattern_b, end_b))
                                        }
                                        // If positions are equal, choose by priority (lower index is better)
                                        core::cmp::Ordering::Equal => {
                                            if pattern_a < pattern_b {
                                                Some((start_a, pattern_a, end_a))
                                            } else {
                                                Some((start_b, pattern_b, end_b))
                                            }
                                        }
                                    }
                                }
                            };
                            Ok(())
                        }
                        Ok(None) => Ok(()),
                        Err(e) => Err(e),
                    }
                });

                chunk_handles.push(handle);
            }

            // Wait for this chunk to complete and collect any errors
            for handle in chunk_handles {
                match handle.join() {
                    Ok(Ok(())) => {}
                    Ok(Err(e)) => return Err(e), // Propagate VM errors
                    Err(e) => {
                        // Thread panicked - this is a serious issue
                        return Err(crate::Error::RuntimeError(
                            crate::RuntimeError::BacktrackLimitExceeded,
                        ));
                    }
                }
            }

            // Early termination: check if we can stop searching
            // If we found a match at the current position and all higher priority patterns
            // have been checked (either already matched or checked in previous chunks)
            let current_best = best_match.lock().unwrap();
            if let Some((start, pattern, _)) = *current_best {
                if start == pos {
                    // Check if all higher priority patterns have been checked
                    let highest_priority_in_remaining = self
                        .hard_patterns
                        .iter()
                        .skip(chunk.len())
                        .map(|hp| hp.index)
                        .min();

                    // If all remaining patterns have lower priority, we can stop
                    if highest_priority_in_remaining
                        .map(|min_idx| min_idx > pattern)
                        .unwrap_or(true)
                    {
                        break;
                    }
                }
            }
        }

        let result = best_match.lock().unwrap();
        Ok(result.map(|(start, pattern, end)| SetMatch {
            pattern,
            match_: Match::new(text, start, end),
        }))
    }

    #[cfg(not(feature = "std"))]
    fn find_hard_match<'t>(&self, text: &'t str, pos: usize) -> Result<Option<SetMatch<'t>>> {
        let mut best_match: Option<SetMatch<'t>> = None;

        for hard_pattern in &self.hard_patterns {
            // Early termination: if we found a match at the starting position with a pattern
            // that has higher priority than the current one, and all patterns with even higher
            // priority than the match have been checked, we can stop
            if let Some(ref m) = best_match {
                if m.start() == pos && m.pattern() < hard_pattern.index {
                    // Check if all higher priority patterns have been checked
                    let all_higher_checked = (0..m.pattern()).all(|i| {
                        // Pattern must have been checked (exists in easy or hard patterns before current position)
                        let in_easy = self.easy_patterns.iter().any(|ep| ep.index == i);
                        let in_hard_before = self
                            .hard_patterns
                            .iter()
                            .take_while(|hp| hp.index != hard_pattern.index)
                            .any(|hp| hp.index == i);
                        in_easy || in_hard_before
                    });
                    if all_higher_checked {
                        break;
                    }
                }
            }

            // Execute the VM to find a match
            if let Some(saves) =
                vm::run(&hard_pattern.prog, text, pos, 0, &hard_pattern.options)?
            {
                let current_match = SetMatch {
                    pattern: hard_pattern.index,
                    match_: Match::new(text, saves[0], saves[1]),
                };

                best_match = match best_match {
                    None => Some(current_match),
                    Some(existing) => Some(Self::choose_best_of_two(existing, current_match)),
                };
            }
        }

        Ok(best_match)
    }

    fn choose_best_match<'t>(
        easy: Option<SetMatch<'t>>,
        hard: Option<SetMatch<'t>>,
    ) -> Option<SetMatch<'t>> {
        match (easy, hard) {
            (None, None) => None,
            (Some(m), None) | (None, Some(m)) => Some(m),
            (Some(easy), Some(hard)) => Some(Self::choose_best_of_two(easy, hard)),
        }
    }

    fn choose_best_of_two<'t>(a: SetMatch<'t>, b: SetMatch<'t>) -> SetMatch<'t> {
        // Choose by position first (lower is better)
        match a.start().cmp(&b.start()) {
            core::cmp::Ordering::Less => a,
            core::cmp::Ordering::Greater => b,
            // If positions are equal, choose by priority (lower index is better)
            core::cmp::Ordering::Equal => {
                if a.pattern < b.pattern {
                    a
                } else {
                    b
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_easy_patterns_only() {
        let set = RegexSet::new(&["foo", "bar", "baz"]).unwrap();
        let text = "hello foo world";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.pattern(), 0);
        assert_eq!(m.as_str(), "foo");
    }

    #[test]
    fn test_priority_order() {
        let set = RegexSet::new(&["bar", "foo"]).unwrap();
        let text = "foobar";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        // "foo" appears first in the text, so it should match
        assert_eq!(m.as_str(), "foo");
        assert_eq!(m.pattern(), 1);
    }

    #[test]
    fn test_same_position_priority() {
        let set = RegexSet::new(&["foo", "f", "fo"]).unwrap();
        let text = "foo";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        // All match at position 0, highest priority wins
        assert_eq!(m.pattern(), 0);
        assert_eq!(m.as_str(), "foo");
    }

    #[test]
    fn test_hard_patterns() {
        let set = RegexSet::new(&[r"(\w+)\1", "foo", "bar"]).unwrap();
        let text = "foobar foofoo";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        // "foo" at position 0 should match first
        assert_eq!(m.as_str(), "foo");
        assert_eq!(m.pattern(), 1);
    }

    #[test]
    fn test_hard_pattern_wins() {
        let set = RegexSet::new(&[r"(\w+)\1", "bar"]).unwrap();
        let text = "foofoo bar";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        // The backreference pattern should match at position 0
        assert_eq!(m.as_str(), "foofoo");
        assert_eq!(m.pattern(), 0);
    }

    #[test]
    fn test_no_match() {
        let set = RegexSet::new(&["foo", "bar"]).unwrap();
        let text = "baz";
        let result = set.find(text).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_mixed_patterns() {
        let set = RegexSet::new(&["easy", r"(\w+)\1", "pattern"]).unwrap();
        let text = "this is a pattern foofoo test";
        let result = set.find(text).unwrap();
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.as_str(), "pattern");
        assert_eq!(m.pattern(), 2);
    }
}
