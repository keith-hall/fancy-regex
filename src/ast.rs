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

//! Raw AST types for regex parsing.
//!
//! This module contains the raw AST that preserves literal constructs
//! from the parsed regex before conversion to the intermediate representation.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

/// Raw AST node representing parsed regex constructs.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ast {
    /// An empty expression
    Empty,
    
    /// Any character `.`
    Dot { 
        /// Whether it matches newlines (affected by flags)
        newline: bool 
    },
    
    /// A literal character or string
    Literal { 
        /// The literal text
        val: String,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Character class like `[a-z]` or `[^0-9]`
    CharClass {
        /// The raw character class string (without brackets)
        content: String,
        /// Whether it's negated `[^...]`
        negated: bool,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Meta character escapes like `\w`, `\s`, `\d`, etc.
    MetaChar {
        /// The meta character (w, s, d, h, v, etc.)
        char: char,
        /// Whether it's negated (uppercase version)
        negated: bool,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Assertions like `^`, `$`, `\b`, `\B`
    Assertion(AssertionKind),
    
    /// Concatenation of expressions
    Concat(Vec<Ast>),
    
    /// Alternation (|) of expressions
    Alt(Vec<Ast>),
    
    /// Capturing group `(...)`
    Group(Box<Ast>),
    
    /// Non-capturing group `(?:...)`
    NonCapturingGroup(Box<Ast>),
    
    /// Named group `(?<name>...)` or `(?P<name>...)`
    NamedGroup {
        /// Group name
        name: String,
        /// The group content
        expr: Box<Ast>,
    },
    
    /// Atomic group `(?>...)`
    AtomicGroup(Box<Ast>),
    
    /// Lookaround assertions
    LookAround {
        /// The expression to look for
        expr: Box<Ast>,
        /// Type of lookaround
        kind: LookAroundKind,
    },
    
    /// Repetition `*`, `+`, `?`, `{n,m}`
    Repeat {
        /// Expression being repeated
        expr: Box<Ast>,
        /// Minimum repetitions
        min: usize,
        /// Maximum repetitions (usize::MAX for unbounded)
        max: usize,
        /// Whether greedy (true) or lazy (false)
        greedy: bool,
    },
    
    /// Backreference `\1`, `\2`, etc.
    Backref {
        /// Group number
        group: usize,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Named backreference `\k<name>` or `(?P=name)`
    NamedBackref {
        /// Group name
        name: String,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Relative backreference with recursion level
    RelativeBackref {
        /// Group number
        group: usize,
        /// Relative recursion level
        relative_level: isize,
        /// Whether case-insensitive matching is enabled
        casei: bool,
    },
    
    /// Flags group `(?flags)` or `(?flags:...)`
    Flags {
        /// Flags to set/unset
        flags: FlagSet,
        /// Optional expression (None for `(?flags)`, Some for `(?flags:...)`)
        expr: Option<Box<Ast>>,
    },
    
    /// Comment `(?# comment)`
    Comment {
        /// Comment text
        text: String,
    },
    
    /// Conditional expression `(?(condition)true|false)`
    Conditional {
        /// Condition to test
        condition: Box<Ast>,
        /// Expression for true branch
        true_branch: Box<Ast>,
        /// Expression for false branch
        false_branch: Box<Ast>,
    },
    
    /// Subroutine call `(?R)`, `(?0)`, `(?&name)`
    SubroutineCall {
        /// Target of the call
        target: SubroutineTarget,
    },
    
    /// Keep-out `\K`
    KeepOut,
    
    /// Continue from previous match end `\G`
    ContinueFromPreviousMatchEnd,
}

/// Types of assertions
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AssertionKind {
    /// Start of text `^`
    StartText,
    /// End of text `$`
    EndText,
    /// Start of line (multiline mode)
    StartLine { crlf: bool },
    /// End of line (multiline mode)  
    EndLine { crlf: bool },
    /// Word boundary `\b`
    WordBoundary,
    /// Not word boundary `\B`
    NotWordBoundary,
    /// Left word boundary
    LeftWordBoundary,
    /// Right word boundary
    RightWordBoundary,
}

/// Types of lookaround assertions
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LookAroundKind {
    /// Positive lookahead `(?=...)`
    LookAhead,
    /// Negative lookahead `(?!...)`
    LookAheadNeg,
    /// Positive lookbehind `(?<=...)`
    LookBehind,
    /// Negative lookbehind `(?<!...)`
    LookBehindNeg,
}

/// Flag settings for a flags group
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FlagSet {
    /// Case insensitive flag `i`
    pub case_insensitive: Option<bool>,
    /// Multiline flag `m`
    pub multiline: Option<bool>,
    /// Dot matches newline flag `s`
    pub dot_matches_newline: Option<bool>,
    /// Ignore whitespace flag `x`
    pub ignore_whitespace: Option<bool>,
    /// Unicode flag `u`
    pub unicode: Option<bool>,
    /// Swap greed flag `U`
    pub swap_greed: Option<bool>,
}

impl FlagSet {
    /// Create an empty flag set
    pub fn new() -> Self {
        FlagSet {
            case_insensitive: None,
            multiline: None,
            dot_matches_newline: None,
            ignore_whitespace: None,
            unicode: None,
            swap_greed: None,
        }
    }
}

impl Default for FlagSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Target of a subroutine call
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SubroutineTarget {
    /// Recursive call to whole pattern `(?R)` or `(?0)`
    Recursive,
    /// Call to numbered group `(?1)`, `(?2)`, etc.
    Group(usize),
    /// Call to named group `(?&name)`
    Named(String),
    /// Relative call `(?-1)`, `(?+1)`, etc.
    Relative(isize),
}

/// Tree containing the parsed AST and metadata
#[derive(Debug, Clone)]
pub struct AstTree {
    /// Root AST node
    pub ast: Ast,
    /// Regex flags in effect
    pub flags: u32,
}