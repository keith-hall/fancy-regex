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

//! RegexSet API for matching multiple patterns against the same input.
//!
//! This module provides [`RegexSet`], which allows efficient matching of multiple
//! regular expression patterns against the same input text. This is particularly
//! useful for applications like syntax highlighting, where many patterns need to
//! be matched against each line of text.
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```rust
//! use fancy_regex::RegexSet;
//!
//! # fn main() -> Result<(), fancy_regex::Error> {
//! let set = RegexSet::new(&[
//!     r"\d+",              // Pattern 0: numbers
//!     r"\w+",              // Pattern 1: words
//!     r"(?<=\$)\d+\.\d+",  // Pattern 2: prices (with lookbehind)
//! ])?;
//!
//! let text = "The price is $29.99 today";
//!
//! for result in set.matches(text) {
//!     let m = result?;
//!     println!("Pattern {} matched '{}' at {}..{}",
//!         m.pattern(), m.as_str(), m.start(), m.end());
//! }
//! # Ok(())
//! # }
//! ```
//!
//! Using the builder for custom options:
//!
//! ```rust
//! use fancy_regex::RegexSetBuilder;
//!
//! # fn main() -> Result<(), fancy_regex::Error> {
//! let set = RegexSetBuilder::new(&[
//!     r"hello",
//!     r"world",
//! ])
//! .case_insensitive(true)
//! .multi_line(true)
//! .build()?;
//!
//! let text = "HELLO\nWORLD";
//!
//! for result in set.matches(text) {
//!     let m = result?;
//!     println!("Pattern {} matched: {}", m.pattern(), m.as_str());
//! }
//! # Ok(())
//! # }
//! ```
//!
//! # Performance
//!
//! The `RegexSet` uses a hybrid approach to achieve good performance:
//!
//! - **Easy patterns** (those without backreferences, lookaround, etc.) are
//!   combined into a single multi-pattern DFA for parallel evaluation. This
//!   provides very fast matching with linear time complexity.
//!
//! - **Hard patterns** (those with backreferences, lookaround, etc.) are
//!   evaluated individually using a backtracking VM. These may have exponential
//!   time complexity in pathological cases.
//!
//! For best performance, try to design patterns that can be delegated to the
//! DFA when possible.
//!
//! # Priority and Non-Overlapping Matches
//!
//! The iterator returns non-overlapping matches in order of their start position.
//! When multiple patterns match at the same position, the pattern with the
//! lowest index (specified first in the constructor) wins. After yielding a match
//! at position `pos` with length `len`, the next match starts searching from
//! `pos + max(1, len)`, which prevents infinite loops on zero-width matches.

use alloc::string::{String, ToString};
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::ops::Range;

use regex_automata::meta::Regex as RaRegex;
use regex_automata::meta::{Builder as RaBuilder, Config as RaConfig};
use regex_automata::util::syntax::Config as SyntaxConfig;
use regex_automata::Input as RaInput;
use regex_automata::{Anchored, PatternSet};

use crate::analyze::{analyze, can_compile_as_anchored};
use crate::compile::compile;
use crate::optimize::optimize;
use crate::parse::NamedGroups;
use crate::vm::{self, Prog};
use crate::{Captures, Expr, RegexOptions, Result};

/// A builder for a `RegexSet` to allow configuring options.
///
/// This builder allows you to configure the compilation options for all patterns
/// in a regex set. All patterns in the set share the same options.
///
/// # Examples
///
/// ```rust
/// use fancy_regex::RegexSetBuilder;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = RegexSetBuilder::new(&[r"hello", r"world"])
///     .case_insensitive(true)
///     .multi_line(true)
///     .backtrack_limit(10_000_000)
///     .build()?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct RegexSetBuilder {
    patterns: Vec<String>,
    syntaxc: SyntaxConfig,
    backtrack_limit: usize,
    delegate_size_limit: Option<usize>,
    delegate_dfa_size_limit: Option<usize>,
    oniguruma_mode: bool,
    max_concurrent_threads: Option<usize>,
}

impl RegexSetBuilder {
    /// Create a new RegexSet builder with a list of patterns.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use fancy_regex::RegexSetBuilder;
    ///
    /// let builder = RegexSetBuilder::new(&[r"\d+", r"[a-z]+"]);
    /// ```
    pub fn new<I, S>(patterns: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let patterns: Vec<String> = patterns
            .into_iter()
            .map(|s| s.as_ref().to_string())
            .collect();
        RegexSetBuilder {
            patterns,
            syntaxc: SyntaxConfig::default(),
            backtrack_limit: 1_000_000,
            delegate_size_limit: None,
            delegate_dfa_size_limit: None,
            oniguruma_mode: false,
            max_concurrent_threads: None,
        }
    }

    /// Set case insensitive mode for all patterns in the set.
    pub fn case_insensitive(&mut self, yes: bool) -> &mut Self {
        self.syntaxc = self.syntaxc.case_insensitive(yes);
        self
    }

    /// Set multi-line mode for all patterns in the set.
    pub fn multi_line(&mut self, yes: bool) -> &mut Self {
        self.syntaxc = self.syntaxc.multi_line(yes);
        self
    }

    /// Allow whitespace to be ignored in patterns.
    pub fn ignore_whitespace(&mut self, yes: bool) -> &mut Self {
        self.syntaxc = self.syntaxc.ignore_whitespace(yes);
        self
    }

    /// Enable or disable the "dot matches any character" flag for all patterns.
    pub fn dot_matches_new_line(&mut self, yes: bool) -> &mut Self {
        self.syntaxc = self.syntaxc.dot_matches_new_line(yes);
        self
    }

    /// Enable or disable Unicode mode for all patterns.
    pub fn unicode_mode(&mut self, yes: bool) -> &mut Self {
        self.syntaxc = self.syntaxc.unicode(yes);
        self
    }

    /// Set the backtracking limit for fancy patterns.
    pub fn backtrack_limit(&mut self, limit: usize) -> &mut Self {
        self.backtrack_limit = limit;
        self
    }

    /// Set size limit for delegated regex compilation.
    pub fn delegate_size_limit(&mut self, limit: usize) -> &mut Self {
        self.delegate_size_limit = Some(limit);
        self
    }

    /// Set DFA size limit for delegated regex compilation.
    pub fn delegate_dfa_size_limit(&mut self, limit: usize) -> &mut Self {
        self.delegate_dfa_size_limit = Some(limit);
        self
    }

    /// Enable Oniguruma compatibility mode for all patterns in the set.
    ///
    /// When enabled, this changes the behavior of certain regex features to match
    /// Oniguruma's behavior more closely.
    pub fn oniguruma_mode(&mut self, yes: bool) -> &mut Self {
        self.oniguruma_mode = yes;
        self
    }

    /// Set maximum number of concurrent threads for hard pattern evaluation.
    /// Only applies when the `std` feature is enabled.
    ///
    /// - `None` (default): Use the number of CPU cores
    /// - `Some(n)`: Use at most `n` threads concurrently
    ///
    /// When `std` feature is disabled, this setting is ignored and all hard
    /// patterns are evaluated sequentially.
    pub fn max_concurrent_threads(&mut self, limit: Option<usize>) -> &mut Self {
        self.max_concurrent_threads = limit;
        self
    }

    /// Build the RegexSet.
    ///
    /// Returns an error if any pattern fails to compile or if resource limits
    /// are exceeded during compilation.
    pub fn build(&self) -> Result<RegexSet> {
        if self.patterns.is_empty() {
            return Ok(RegexSet {
                inner: Arc::new(RegexSetImpl {
                    easy_patterns: None,
                    hard_patterns: Vec::new(),
                    backtrack_limit: self.backtrack_limit,
                }),
            });
        }

        let mut easy_pattern_strings = Vec::new();
        let mut easy_pattern_infos = Vec::new();
        let mut hard_patterns = Vec::new();

        // Compute flags once before the loop
        let flags = self.compute_flags();

        // Parse, analyze, and classify each pattern
        for (index, pattern) in self.patterns.iter().enumerate() {
            let mut expr_tree = Expr::parse_tree_with_flags(pattern, flags)?;

            // Try to optimize the expression tree
            let requires_capture_group_fixup = optimize(&mut expr_tree);
            let info = analyze(&expr_tree, requires_capture_group_fixup)?;

            if info.hard {
                // Hard pattern - compile to VM program
                let prog = compile(&info, can_compile_as_anchored(&expr_tree.expr))?;
                let pattern_options = RegexOptions {
                    pattern: pattern.clone(),
                    syntaxc: self.syntaxc,
                    backtrack_limit: self.backtrack_limit,
                    delegate_size_limit: self.delegate_size_limit,
                    delegate_dfa_size_limit: self.delegate_dfa_size_limit,
                    oniguruma_mode: self.oniguruma_mode,
                };
                hard_patterns.push(HardPattern {
                    pattern_id: index,
                    prog: Arc::new(prog),
                    n_groups: info.end_group(),
                    named_groups: Arc::new(expr_tree.named_groups),
                    options: pattern_options,
                });
            } else {
                // Easy pattern - extract delegate string for DFA
                let mut delegate_str = String::new();
                expr_tree.expr.to_str(&mut delegate_str, 0);
                easy_pattern_strings.push(delegate_str.clone());
                easy_pattern_infos.push(EasyPatternInfo {
                    pattern_id: index,
                    delegate_pattern: delegate_str,
                    named_groups: Arc::new(expr_tree.named_groups),
                    explicit_capture_group_0: requires_capture_group_fixup,
                });
            }
        }

        // Build multi-pattern DFA for easy patterns
        let easy_patterns = if !easy_pattern_strings.is_empty() {
            let mut config = RaConfig::new();
            if let Some(limit) = self.delegate_size_limit {
                config = config.nfa_size_limit(Some(limit));
            }
            if let Some(limit) = self.delegate_dfa_size_limit {
                config = config.dfa_size_limit(Some(limit));
            }

            let dfa = RaBuilder::new()
                .configure(config)
                .syntax(self.syntaxc)
                .build_many(&easy_pattern_strings)
                .map_err(crate::CompileError::InnerError)?;

            Some(EasyPatternSet {
                dfa,
                patterns: easy_pattern_infos,
            })
        } else {
            None
        };

        Ok(RegexSet {
            inner: Arc::new(RegexSetImpl {
                easy_patterns,
                hard_patterns,
                backtrack_limit: self.backtrack_limit,
            }),
        })
    }

    fn compute_flags(&self) -> u32 {
        use crate::parse_flags::*;

        let insensitive = if self.syntaxc.get_case_insensitive() {
            FLAG_CASEI
        } else {
            0
        };
        let multiline = if self.syntaxc.get_multi_line() {
            FLAG_MULTI
        } else {
            0
        };
        let whitespace = if self.syntaxc.get_ignore_whitespace() {
            FLAG_IGNORE_SPACE
        } else {
            0
        };
        let dotnl = if self.syntaxc.get_dot_matches_new_line() {
            FLAG_DOTNL
        } else {
            0
        };
        let unicode = if self.syntaxc.get_unicode() {
            FLAG_UNICODE
        } else {
            0
        };
        let oniguruma_mode = if self.oniguruma_mode {
            FLAG_ONIGURUMA_MODE
        } else {
            0
        };

        insensitive | multiline | whitespace | dotnl | unicode | oniguruma_mode
    }
}

/// A compiled set of regular expressions.
///
/// A `RegexSet` allows you to match multiple patterns against the same input
/// text efficiently. It's particularly useful for applications like syntax
/// highlighting or token scanning where you need to check many patterns against
/// each piece of text.
///
/// The set analyzes patterns at compile time and uses different strategies for
/// different types of patterns:
/// - Simple patterns are combined into a single high-performance DFA
/// - Complex patterns (with backreferences, lookaround, etc.) use backtracking
///
/// # Examples
///
/// Basic matching:
///
/// ```rust
/// use fancy_regex::RegexSet;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = RegexSet::new(&[r"\d+", r"[a-z]+", r"[A-Z]+"])?;
///
/// let text = "abc 123 XYZ";
/// for m in set.matches(text) {
///     let m = m?;
///     println!("Pattern {} matched: {}", m.pattern(), m.as_str());
/// }
/// # Ok(())
/// # }
/// ```
///
/// The `RegexSet` is cheaply cloneable (via `Arc`) and can be used from
/// multiple threads:
///
/// ```rust
/// use fancy_regex::RegexSet;
/// use std::sync::Arc;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = Arc::new(RegexSet::new(&[r"\d+"])?);
/// let set_clone = Arc::clone(&set);
///
/// // Use from different threads...
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
pub struct RegexSet {
    inner: Arc<RegexSetImpl>,
}

#[derive(Debug)]
struct RegexSetImpl {
    easy_patterns: Option<EasyPatternSet>,
    hard_patterns: Vec<HardPattern>,
    #[allow(dead_code)]
    backtrack_limit: usize,
}

#[derive(Debug)]
struct EasyPatternSet {
    dfa: RaRegex,
    patterns: Vec<EasyPatternInfo>,
}

#[derive(Debug)]
struct EasyPatternInfo {
    pattern_id: usize,
    delegate_pattern: String,
    named_groups: Arc<NamedGroups>,
    explicit_capture_group_0: bool,
}

#[derive(Debug)]
struct HardPattern {
    pattern_id: usize,
    prog: Arc<Prog>,
    #[allow(dead_code)]
    n_groups: usize,
    named_groups: Arc<NamedGroups>,
    options: RegexOptions,
}

impl RegexSet {
    /// Create a new RegexSet from an iterator of patterns using default options.
    ///
    /// All patterns will use the same default configuration:
    /// - Case sensitive
    /// - Multi-line mode disabled
    /// - Dot does not match newline
    /// - Unicode mode enabled
    ///
    /// # Errors
    ///
    /// Returns an error if any pattern fails to compile.
    pub fn new<I, S>(patterns: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        RegexSetBuilder::new(patterns).build()
    }

    /// Returns the number of patterns in the set.
    pub fn len(&self) -> usize {
        let easy_count = self
            .inner
            .easy_patterns
            .as_ref()
            .map(|e| e.patterns.len())
            .unwrap_or(0);
        easy_count + self.inner.hard_patterns.len()
    }

    /// Returns true if the set contains no patterns.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create a new matches iterator for the given haystack.
    ///
    /// The iterator will find all non-overlapping matches in the haystack,
    /// returning them in order of their start position. When multiple patterns
    /// match at the same position, the pattern with the lowest index wins.
    ///
    /// # Arguments
    ///
    /// * `haystack` - The text to search in
    ///
    /// # Returns
    ///
    /// An iterator over matches in the haystack.
    pub fn matches<'h>(&'h self, haystack: &'h str) -> RegexSetMatches<'h> {
        self.matches_range(haystack, 0..haystack.len())
    }

    /// Create a new matches iterator with a specific byte range in the haystack.
    ///
    /// This is useful when you want to search only a portion of the haystack while
    /// still having access to the full text for features like lookbehind/lookahead.
    ///
    /// # Arguments
    ///
    /// * `haystack` - The full text
    /// * `range` - The byte range within the haystack to search
    ///
    /// # Panics
    ///
    /// Panics if the range is not within bounds or does not fall on UTF-8 boundaries.
    pub fn matches_range<'h>(
        &'h self,
        haystack: &'h str,
        range: Range<usize>,
    ) -> RegexSetMatches<'h> {
        assert!(range.start <= haystack.len() && range.end <= haystack.len());
        assert!(haystack.is_char_boundary(range.start) && haystack.is_char_boundary(range.end));

        RegexSetMatches {
            set: self,
            haystack,
            range: range.clone(),
            current_pos: range.start,
            easy_next_match: None,
            hard_cache: Vec::new(),
        }
    }
}

/// A match from a RegexSet, including the pattern index and capture groups.
///
/// This type represents a single match found by a [`RegexSet`]. It provides
/// information about which pattern matched, the location of the match, and
/// access to any capture groups.
///
/// # Examples
///
/// ```rust
/// use fancy_regex::RegexSet;
///
/// # fn main() -> Result<(), fancy_regex::Error> {
/// let set = RegexSet::new(&[r"(\d+)-(\d+)", r"[a-z]+"])?;
/// let text = "abc 123-456";
///
/// for m in set.matches(text) {
///     let m = m?;
///     println!("Pattern {} matched '{}' at {}..{}",
///         m.pattern(), m.as_str(), m.start(), m.end());
///     
///     // Access capture groups
///     for cap in m.captures().iter() {
///         if let Some(cap) = cap {
///             println!("  Capture: {}", cap.as_str());
///         }
///     }
/// }
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct RegexSetMatch<'h> {
    pattern_index: usize,
    captures: Captures<'h>,
}

impl<'h> RegexSetMatch<'h> {
    /// Returns the index of the pattern that matched.
    pub fn pattern(&self) -> usize {
        self.pattern_index
    }

    /// Returns the start byte offset of the overall match.
    pub fn start(&self) -> usize {
        self.captures.get(0).map(|m| m.start()).unwrap_or(0)
    }

    /// Returns the end byte offset of the overall match.
    pub fn end(&self) -> usize {
        self.captures.get(0).map(|m| m.end()).unwrap_or(0)
    }

    /// Returns the matched text.
    pub fn as_str(&self) -> &'h str {
        self.captures.get(0).map(|m| m.as_str()).unwrap_or("")
    }

    /// Returns the range of the overall match.
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    /// Returns a reference to the captures for this match.
    pub fn captures(&self) -> &Captures<'h> {
        &self.captures
    }

    /// Consumes self and returns the captures.
    pub fn into_captures(self) -> Captures<'h> {
        self.captures
    }
}

/// Iterator over matches in a haystack.
///
/// The iterator returns non-overlapping matches in order of their start position.
/// After yielding a match at position `pos` with length `len`, the next match
/// will start searching from `pos + max(1, len)`. This prevents infinite loops
/// on zero-width matches while still allowing them to be found.
///
/// When multiple patterns match at the same position, the pattern with the
/// lowest index (specified first in the constructor) is returned.
#[derive(Debug)]
pub struct RegexSetMatches<'h> {
    set: &'h RegexSet,
    haystack: &'h str,
    range: Range<usize>,
    current_pos: usize,
    // For easy patterns: cache next match position (if found)
    easy_next_match: Option<(usize, Range<usize>)>, // (pattern_idx, range)
    hard_cache: Vec<Option<(Range<usize>, Captures<'h>)>>,
}

impl<'h> Iterator for RegexSetMatches<'h> {
    type Item = Result<RegexSetMatch<'h>>;

    /// Returns the next match, or None if no more matches exist.
    ///
    /// Returns an error if:
    /// - A hard pattern exceeds its backtracking limit
    /// - Any other runtime error occurs during matching
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_pos > self.range.end {
            return None;
        }

        // Search hard patterns if we haven't cached them
        if self.hard_cache.is_empty() && !self.set.inner.hard_patterns.is_empty() {
            self.hard_cache = vec![None; self.set.inner.hard_patterns.len()];
        }

        // Find earliest match across all patterns
        loop {
            let mut earliest_match: Option<(usize, usize, RegexSetMatch<'h>)> = None;

            // Search for next easy pattern match if we don't have one cached
            if self.easy_next_match.is_none() {
                if let Some(ref easy_set) = self.set.inner.easy_patterns {
                    let input = RaInput::new(self.haystack)
                        .range(self.range.clone())
                        .span(self.current_pos..self.range.end);

                    // Find the next match from current position
                    if let Some(mat) = easy_set.dfa.find(input) {
                        let pattern_idx = mat.pattern().as_usize();
                        let range = mat.start()..mat.end();
                        self.easy_next_match = Some((pattern_idx, range));
                    }
                }
            }

            // Check if easy pattern match is at or after current position
            if let Some((pattern_idx, ref range)) = self.easy_next_match {
                if range.start >= self.current_pos {
                    // Need to check which patterns match at this position
                    match self.find_easy_match_at_position(range.start, pattern_idx, range) {
                        Ok(Some(m)) => {
                            earliest_match = Some((m.start(), m.pattern(), m));
                        }
                        Ok(None) => {
                            // No match found, invalidate cache and try again
                            self.easy_next_match = None;
                            continue;
                        }
                        Err(e) => return Some(Err(e)),
                    }
                } else {
                    // This match is behind us, invalidate it and search again
                    self.easy_next_match = None;
                    continue;
                }
            }

            // Check hard patterns - search each if not cached
            for (i, hard_pattern) in self.set.inner.hard_patterns.iter().enumerate() {
                if self.hard_cache[i].is_none() {
                    match self.search_hard_pattern(i, hard_pattern) {
                        Ok(result) => {
                            self.hard_cache[i] = result;
                        }
                        Err(e) => return Some(Err(e)),
                    }
                }

                if let Some((ref range, ref captures)) = self.hard_cache[i] {
                    if range.start >= self.current_pos {
                        let pattern_id = hard_pattern.pattern_id;
                        let key = (range.start, pattern_id);
                        if earliest_match.is_none()
                            || key
                                < (
                                    earliest_match.as_ref().unwrap().0,
                                    earliest_match.as_ref().unwrap().1,
                                )
                        {
                            earliest_match = Some((
                                range.start,
                                pattern_id,
                                RegexSetMatch {
                                    pattern_index: pattern_id,
                                    captures: captures.clone(),
                                },
                            ));
                        }
                    }
                }
            }

            match earliest_match {
                Some((_, _, match_result)) => {
                    // Advance position for next iteration
                    let match_len = match_result.end() - match_result.start();
                    self.current_pos = match_result.end() + if match_len == 0 { 1 } else { 0 };

                    // Invalidate cache entries that are now behind us
                    self.invalidate_cache_before(self.current_pos);

                    return Some(Ok(match_result));
                }
                None => {
                    // No matches found
                    return None;
                }
            }
        }
    }
}

impl<'h> RegexSetMatches<'h> {
    /// Find the best match among easy patterns at a specific position.
    /// At the given position, checks which patterns match and returns the one with lowest index.
    fn find_easy_match_at_position(
        &self,
        pos: usize,
        cached_pattern_idx: usize,
        cached_range: &Range<usize>,
    ) -> Result<Option<RegexSetMatch<'h>>> {
        if let Some(ref easy_set) = self.set.inner.easy_patterns {
            // Use which_overlapping_matches to find all patterns that match starting at pos
            let input = RaInput::new(self.haystack)
                .range(self.range.clone())
                .span(pos..self.range.end);

            let mut pattern_set = PatternSet::new(easy_set.patterns.len());
            easy_set
                .dfa
                .which_overlapping_matches(&input, &mut pattern_set);

            // Find the pattern with the lowest index that matches at this position
            if let Some(pattern_id) = pattern_set.iter().next() {
                let pattern_idx = pattern_id.as_usize();
                let pattern_info = &easy_set.patterns[pattern_idx];

                // Check if this is the cached pattern - if so, we already have the range
                let range = if pattern_idx == cached_pattern_idx && cached_range.start == pos {
                    cached_range.clone()
                } else {
                    // Need to find the actual match range for this pattern
                    let search_input = RaInput::new(self.haystack)
                        .range(self.range.clone())
                        .span(pos..self.range.end);

                    // Search for this specific pattern
                    let mut found_range = None;
                    for mat in easy_set.dfa.find_iter(search_input) {
                        if mat.pattern().as_usize() == pattern_idx && mat.start() == pos {
                            found_range = Some(mat.start()..mat.end());
                            break;
                        }
                        // If we found a match but it doesn't start at pos, we're done
                        if mat.start() > pos {
                            break;
                        }
                    }

                    match found_range {
                        Some(r) => r,
                        None => return Ok(None),
                    }
                };

                let captures = self.extract_easy_captures(pattern_info, &range)?;

                return Ok(Some(RegexSetMatch {
                    pattern_index: pattern_info.pattern_id,
                    captures,
                }));
            }
        }

        Ok(None)
    }

    fn extract_easy_captures(
        &self,
        info: &EasyPatternInfo,
        range: &Range<usize>,
    ) -> Result<Captures<'h>> {
        // Build a regex for this specific pattern to extract captures
        let config = RaConfig::new();
        let extractor = RaBuilder::new()
            .configure(config)
            .syntax(SyntaxConfig::default()) // Use same syntax config as when building
            .build(&info.delegate_pattern)
            .map_err(crate::CompileError::InnerError)?;

        let input = RaInput::new(self.haystack)
            .range(range.clone())
            .anchored(Anchored::Yes);

        let mut ra_captures = extractor.create_captures();
        let _ = extractor.search_captures(&input, &mut ra_captures);

        // Convert to fancy-regex Captures
        Ok(Captures::from_regex_automata(
            self.haystack,
            ra_captures,
            info.explicit_capture_group_0,
            Arc::clone(&info.named_groups),
        ))
    }

    fn search_hard_pattern(
        &self,
        _index: usize,
        pattern: &HardPattern,
    ) -> Result<Option<(Range<usize>, Captures<'h>)>> {
        match vm::run(
            &pattern.prog,
            self.haystack,
            self.current_pos,
            0,
            &pattern.options,
        )? {
            Some(saves) => {
                let start = saves[0];
                let end = saves[1];
                let captures =
                    Captures::from_saves(self.haystack, saves, Arc::clone(&pattern.named_groups));
                Ok(Some((start..end, captures)))
            }
            None => Ok(None),
        }
    }

    fn invalidate_cache_before(&mut self, pos: usize) {
        // Remove easy match if it's before pos
        if let Some((_, ref range)) = self.easy_next_match {
            if range.start < pos {
                self.easy_next_match = None;
            }
        }

        // Remove hard cache entries before pos
        for cached in &mut self.hard_cache {
            if let Some((range, _)) = cached {
                if range.start < pos {
                    *cached = None;
                }
            }
        }
    }
}
