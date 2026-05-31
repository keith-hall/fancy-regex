// Copyright 2026 The Fancy Regex Authors.
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
//! // TODO: get matches and show which pattern matched where
//! # Ok(())
//! # }
//! ```
//!
//! # Performance
//!
//! The `RegexSet` uses a hybrid approach to achieve good performance:
//!
//! A multi-pattern DFA is built for parallel evaluation, to provide very fast
//! candidate position matching with linear time complexity.
//!
//! At the earliest candidate position:
//! - any **hard patterns** (those with backreferences lookaround, etc.) which could
//!   match there are evaluated individually using a backtracking VM in anchored mode.
//!   These may have exponential time complexity in pathological cases.
//! - any **Easy patterns** (those without backreferences, lookaround, etc.) which do
//!   match are also individually run through the underlying regex crate, to resolve
//!   capture groups etc. which may have been skipped in multi-DFA mode.
//!
//! For best performance, try to design patterns that can be fully delegated to the
//! DFA when possible.
//!
//! # Priority and Non-Overlapping Matches
//!
//! There is deliberately no iterator API per se, instead the idea is that the caller
//! would see the list of patterns which match, and the matches themselves, and decide
//! which gets priority.
//! This allows for most flexibility, without having to cater for various scenarios in
//! the RegexSet itself. The `Input` struct makes it easy for the caller to advance
//! position in case of an empty match winning, and the `RegexInput` struct sits on top
//! of that to make it possible to specify rules like whether `\G` should match or not.

use alloc::boxed::Box;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;

use crate::CompileOptions;
use crate::Input;
use crate::RegexInput;
use crate::RegexOptionsBuilder;

use regex_automata::meta::Config as RaConfig;
use regex_automata::meta::Regex as RaRegex;
use regex_automata::util::syntax::Config as SyntaxConfig;
use regex_automata::Anchored;
use regex_automata::Input as RaInput;
use regex_automata::MatchKind;
use regex_automata::PatternSet;

use crate::compile::options_to_rabuilder;
use crate::vm::OPTION_ANCHORED;
use crate::CompileError;
use crate::Error;
use crate::{BytesMode, Captures, Regex, Result};

#[derive(Clone, Debug)]
/// RegexSet API for matching multiple patterns against the same input.
pub struct RegexSet {
    regexes: Vec<Arc<Regex>>,
    candidate_position_finder: RaRegex,
}

#[derive(Clone, Debug, Default)]
/// Configuration for a RegexSet
pub struct RegexSetConfig {
    syntaxc: SyntaxConfig, // TODO: maybe rather than derive Default, make it explicit or base it on RegexOptions' default, for unicode etc
    delegate_size_limit: Option<usize>,
    delegate_dfa_size_limit: Option<usize>,
    bytes_mode: BytesMode,
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
        let builder = RegexOptionsBuilder::new();
        Self::new_with_options(patterns, &builder)
    }

    /// Create a new RegexSet from an iterator of patterns using specified options.
    ///
    /// # Errors
    ///
    /// Returns an error if any pattern fails to compile.
    pub fn new_with_options<I, S>(
        patterns: I,
        options_builder: &RegexOptionsBuilder,
    ) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let regexes = patterns
            .into_iter()
            .map(|pattern| {
                options_builder
                    .build(pattern.as_ref().to_string())
                    .map(Arc::new)
            })
            .collect::<Result<Vec<_>>>()?;

        let config = RegexSetConfig {
            syntaxc: options_builder.options.syntaxc,
            delegate_size_limit: options_builder.options.delegate_size_limit,
            delegate_dfa_size_limit: options_builder.options.delegate_dfa_size_limit,
            bytes_mode: options_builder.options.bytes_mode,
        };
        Self::from_regexes(regexes, config)
    }

    /// Create a new RegexSet from pre-built `Arc<Regex>` instances.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use fancy_regex::{Regex, RegexBuilder, RegexSet, RegexSetConfig};
    /// use std::sync::Arc;
    ///
    /// # fn main() -> Result<(), fancy_regex::Error> {
    /// // Create regexes with different options
    /// let re1 = Arc::new(RegexBuilder::new(r"hello")
    ///     .case_insensitive(true)
    ///     .build()?);
    /// let re2 = Arc::new(Regex::new(r"\d+")?);
    /// let re3 = Arc::new(Regex::new(r"(?<=\w)end")?); // lookbehind - fancy pattern
    ///
    /// // Combine them into a RegexSet
    /// let set = RegexSet::from_regexes([re1, re2, re3], Default::default())?;
    ///
    /// let text = "HELLO 123 send";
    /// // TODO: show matches
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error if the multi-pattern DFA construction fails.
    pub fn from_regexes<I>(regexes: I, config: RegexSetConfig) -> Result<Self>
    where
        I: IntoIterator<Item = Arc<Regex>>,
    {
        let regexes_vec: Vec<Arc<Regex>> = regexes.into_iter().collect();

        let mut patterns = Vec::with_capacity(regexes_vec.len());

        for regex in &regexes_vec {
            patterns.push(regex.seek_pattern());
        }

        let mut builder = options_to_rabuilder(&CompileOptions {
            bytes_mode: config.bytes_mode,
            unicode: config.syntaxc.get_unicode() && !matches!(config.bytes_mode, BytesMode::Ascii),
            delegate_size_limit: config.delegate_size_limit,
            delegate_dfa_size_limit: config.delegate_dfa_size_limit,
            ..CompileOptions::default()
        });
        builder.configure(RaConfig::new().match_kind(MatchKind::All));
        let finder = builder
            .build_many(&patterns)
            .map_err(CompileError::InnerError)
            .map_err(|e| Error::CompileError(Box::new(e)))?;

        Ok(Self {
            regexes: regexes_vec,
            candidate_position_finder: finder,
        })
    }

    /// Returns the number of patterns in the set.
    pub fn len(&self) -> usize {
        self.regexes.len()
    }

    /// Returns true if the set contains no patterns.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator over matches at the earliest match position - if any.
    /// Iterator yields matches in pattern index order
    pub fn find_input<'r, 't, S: Input + ?Sized>(
        &'r self,
        input: RegexInput<'t, S>,
    ) -> Result<Option<RegexSetMatchesAt<'r, 't, S>>> {
        if input.is_done() {
            return Ok(None);
        }

        let haystack = input.haystack();
        let match_range = input.get_range();
        let mut search_start = input.effective_start();

        while search_start <= match_range.end {
            let Some(candidate) = self
                .candidate_position_finder
                .search(&RaInput::new(haystack.as_bytes()).range(search_start..match_range.end))
            else {
                return Ok(None);
            };
            let match_start = candidate.start();
            let mut candidate_patterns = PatternSet::new(self.regexes.len());
            self.candidate_position_finder.which_overlapping_matches(
                &RaInput::new(haystack.as_bytes())
                    .anchored(Anchored::Yes)
                    .range(match_start..match_range.end),
                &mut candidate_patterns,
            );

            let mut matches = Vec::new();
            for pattern_id in candidate_patterns.iter() {
                let pattern_index = pattern_id.as_usize();
                let mut candidate_input = RegexInput::new(haystack)
                    .range(match_range.clone())
                    .from_pos(match_start);
                if input.start_text_override() == Some(false) {
                    candidate_input = candidate_input.start_text(false);
                }
                if input.end_text_override() == Some(false) {
                    candidate_input = candidate_input.end_text(false);
                }
                if input.continue_from_previous_match_end_override() == Some(false) {
                    candidate_input = candidate_input.continue_from_previous_match_end(false);
                }
                if let Some(captures) = self.regexes[pattern_index]
                    .captures_input_with_option_flags(&candidate_input, OPTION_ANCHORED)?
                {
                    matches.push(RegexSetMatch {
                        pattern_index,
                        captures,
                    });
                }
            }

            if !matches.is_empty() {
                return Ok(Some(RegexSetMatchesAt {
                    regex_set: self,
                    haystack,
                    match_start,
                    matches: matches.into_iter(),
                }));
            }

            search_start = haystack.advance_position(match_start);
        }

        Ok(None)
    }
}

/// A match from a RegexSet, including the pattern index and capture groups.
///
/// This type represents a single match found by a [`RegexSet`]. It provides
/// information about which pattern matched, the location of the match, and
/// access to any capture groups.
///
/// TODO: add Examples
#[derive(Debug)]
pub struct RegexSetMatch<'t, S: Input + ?Sized> {
    pattern_index: usize,
    captures: Captures<'t, S>,
}

impl<'t, S: Input + ?Sized> RegexSetMatch<'t, S> {
    /// Returns the pattern index that matched.
    pub fn pattern(&self) -> usize {
        self.pattern_index
    }

    /// Returns the full set of capture groups for this match.
    pub fn captures(&self) -> &Captures<'t, S> {
        &self.captures
    }

    /// Returns the full match.
    pub fn get(&self) -> S::Match<'t> {
        self.captures
            .get(0)
            .expect("`RegexSetMatch` must always contain the overall match")
    }

    /// Returns the start offset of the full match.
    pub fn start(&self) -> usize {
        self.captures
            .get_span(0)
            .expect("`RegexSetMatch` must always contain the overall match")
            .0
    }

    /// Returns the end offset of the full match.
    pub fn end(&self) -> usize {
        self.captures
            .get_span(0)
            .expect("`RegexSetMatch` must always contain the overall match")
            .1
    }
}

impl<'t> RegexSetMatch<'t, str> {
    /// Returns the matched text.
    pub fn as_str(&self) -> &'t str {
        self.captures
            .get(0)
            .expect("`RegexSetMatch` must always contain the overall match")
            .as_str()
    }
}

#[derive(Debug)]
pub struct RegexSetMatchesAt<'r, 't, S: Input + ?Sized> {
    regex_set: &'r RegexSet,
    haystack: &'t S,
    match_start: usize,
    matches: alloc::vec::IntoIter<RegexSetMatch<'t, S>>,
}

impl<'r, 't, S: Input + ?Sized> RegexSetMatchesAt<'r, 't, S> {
    /// Returns the originating regex set.
    pub fn regex_set(&self) -> &'r RegexSet {
        self.regex_set
    }

    /// Returns the searched haystack.
    pub fn haystack(&self) -> &'t S {
        self.haystack
    }

    /// Returns the earliest start position shared by these matches.
    pub fn start(&self) -> usize {
        self.match_start
    }
}

impl<'r, 't, S: Input + ?Sized> Iterator for RegexSetMatchesAt<'r, 't, S> {
    type Item = Result<RegexSetMatch<'t, S>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.matches.next().map(Ok)
    }
}

#[cfg(test)]
mod tests {
    use super::RegexSet;
    use crate::RegexInput;

    #[test]
    fn find_input_returns_all_matches_at_earliest_position_in_pattern_order() {
        let set = RegexSet::new(&[r"\d+", r"\w+", r"(?<=\$)\d+\.\d+"]).unwrap();
        let mut matches = set.find_input(RegexInput::new("$29.99")).unwrap().unwrap();

        let first = matches.next().unwrap().unwrap();
        assert_eq!(0, first.pattern());
        assert_eq!(1, first.start());
        assert_eq!(3, first.end());
        assert_eq!("29", first.as_str());

        let second = matches.next().unwrap().unwrap();
        assert_eq!(1, second.pattern());
        assert_eq!(1, second.start());
        assert_eq!(3, second.end());
        assert_eq!("29", second.as_str());

        let third = matches.next().unwrap().unwrap();
        assert_eq!(2, third.pattern());
        assert_eq!(1, third.start());
        assert_eq!(6, third.end());
        assert_eq!("29.99", third.as_str());

        assert!(matches.next().is_none());
    }

    #[test]
    fn find_input_skips_false_positive_candidate_positions() {
        let set = RegexSet::new(&[r"(?<=foo)bar"]).unwrap();
        let mut matches = set
            .find_input(RegexInput::new("barfoobar"))
            .unwrap()
            .unwrap();

        let only = matches.next().unwrap().unwrap();
        assert_eq!(0, only.pattern());
        assert_eq!(6, only.start());
        assert_eq!(9, only.end());
        assert_eq!("bar", only.as_str());
        assert!(matches.next().is_none());
    }
}
