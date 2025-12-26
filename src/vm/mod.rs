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

//! Backtracking VM for implementing fancy regexes.
//!
//! Read <https://swtch.com/~rsc/regexp/regexp2.html> for a good introduction for how this works.
//!
//! The VM executes a sequence of instructions (a program) against an input string. It keeps track
//! of a program counter (PC) and an index into the string (IX). Execution can have one or more
//! threads.
//!
//! One of the basic instructions is `Lit`, which matches a string against the input. If it matches,
//! the PC advances to the next instruction and the IX to the position after the matched string.
//! If not, the current thread is stopped because it failed.
//!
//! If execution reaches an `End` instruction, the program is successful because a match was found.
//! If there are no more threads to execute, the program has failed to match.
//!
//! A very simple program for the regex `a`:
//!
//! ```text
//! 0: Lit("a")
//! 1: End
//! ```
//!
//! The `Split` instruction causes execution to split into two threads. The first thread is executed
//! with the current string index. If it fails, we reset the string index and resume execution with
//! the second thread. That is what "backtracking" refers to. In order to do that, we keep a stack
//! of threads (PC and IX) to try.
//!
//! Example program for the regex `ab|ac`:
//!
//! ```text
//! 0: Split(1, 4)
//! 1: Lit("a")
//! 2: Lit("b")
//! 3: Jmp(6)
//! 4: Lit("a")
//! 5: Lit("c")
//! 6: End
//! ```
//!
//! The `Jmp` instruction causes execution to jump to the specified instruction. In the example it
//! is needed to separate the two threads.
//!
//! Let's step through execution with that program for the input `ac`:
//!
//! 1. We're at PC 0 and IX 0
//! 2. `Split(1, 4)` means we save a thread with PC 4 and IX 0 for trying later
//! 3. Continue at `Lit("a")` which matches, so we advance IX to 1
//! 4. `Lit("b")` doesn't match at IX 1 (`"b" != "c"`), so the thread fails
//! 5. We continue with the previously saved thread at PC 4 and IX 0 (backtracking)
//! 6. Both `Lit("a")` and `Lit("c")` match and we reach `End` -> successful match (index 0 to 2)

use alloc::string::String;
#[cfg(feature = "variable-lookbehinds")]
use alloc::sync::Arc;
use regex_automata::meta::Regex;

#[cfg(feature = "variable-lookbehinds")]
use regex_automata::util::pool::Pool;

use crate::{Assertion, Formatter};

mod execution;
mod helpers;

// Re-export public items from execution
pub(crate) use execution::run;
pub use execution::{run_default, run_trace};

#[cfg(feature = "variable-lookbehinds")]
pub(crate) use execution::State;

#[cfg(feature = "variable-lookbehinds")]
pub(crate) type CachePoolFn = alloc::boxed::Box<
    dyn Fn() -> regex_automata::hybrid::dfa::Cache
        + Send
        + Sync
        + core::panic::UnwindSafe
        + core::panic::RefUnwindSafe,
>;

/// Enable tracing of VM execution. Only for debugging/investigating.
const OPTION_TRACE: u32 = 1 << 0;
/// When iterating over all matches within a text (e.g. with `find_iter`), empty matches need to be
/// handled specially. If we kept matching at the same position, we'd never stop. So what we do
/// after we've had an empty match, is to advance the position where matching is attempted.
/// If `\G` is used in the pattern, that means it no longer matches. If we didn't tell the VM about
/// the fact that we skipped because of an empty match, it would still treat `\G` as matching. So
/// this option is for communicating that to the VM. Phew.
pub(crate) const OPTION_SKIPPED_EMPTY_MATCH: u32 = 1 << 1;

// TODO: make configurable
pub(super) const MAX_STACK: usize = 1_000_000;

/// Represents a range of capture groups by storing the first and last group numbers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CaptureGroupRange(pub usize, pub usize);

impl CaptureGroupRange {
    /// Returns the start (first) group number.
    pub fn start(&self) -> usize {
        self.0
    }

    /// Returns the end (last) group number.
    pub fn end(&self) -> usize {
        self.1
    }

    /// Converts this range to an Option, returning None if start equals end (no capture groups).
    pub fn to_option_if_non_empty(self) -> Option<Self> {
        if self.start() == self.end() {
            None
        } else {
            Some(self)
        }
    }
}

#[derive(Clone)]
/// Delegate matching to the regex crate
pub struct Delegate {
    /// The regex
    pub inner: Regex,
    /// The regex pattern as a string
    pub pattern: String,
    /// The range of capture groups. None if there are no capture groups.
    pub capture_groups: Option<CaptureGroupRange>,
}

impl core::fmt::Debug for Delegate {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        // Ensures it fails to compile if the struct changes
        let Self {
            inner: _,
            pattern,
            capture_groups,
        } = self;

        f.debug_struct("Delegate")
            .field("pattern", pattern)
            .field("capture_groups", capture_groups)
            .finish()
    }
}

#[cfg(feature = "variable-lookbehinds")]
/// Delegate matching in reverse to regex-automata
pub struct ReverseBackwardsDelegate {
    /// The regex pattern as a string which will be matched in reverse, in a backwards direction
    pub pattern: String,
    /// The delegate regex to match backwards (wrapped in Arc for efficient cloning)
    pub(crate) dfa: Arc<regex_automata::hybrid::dfa::DFA>,
    /// Cache pool for DFA searches
    pub(crate) cache_pool: Pool<regex_automata::hybrid::dfa::Cache, CachePoolFn>,
    /// The forward regex for capture group extraction
    pub(crate) capture_group_extraction_inner: Option<Regex>,
    /// The range of capture groups. None if there are no capture groups.
    pub capture_groups: Option<CaptureGroupRange>,
}

#[cfg(feature = "variable-lookbehinds")]
impl Clone for ReverseBackwardsDelegate {
    fn clone(&self) -> Self {
        let dfa_for_closure = Arc::clone(&self.dfa);
        let create: CachePoolFn = alloc::boxed::Box::new(move || dfa_for_closure.create_cache());
        Self {
            pattern: self.pattern.clone(),
            cache_pool: Pool::new(create),
            dfa: Arc::clone(&self.dfa),
            capture_group_extraction_inner: self.capture_group_extraction_inner.clone(),
            capture_groups: self.capture_groups,
        }
    }
}

#[cfg(feature = "variable-lookbehinds")]
impl core::fmt::Debug for ReverseBackwardsDelegate {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        // Ensures it fails to compile if the struct changes
        let Self {
            pattern,
            dfa: _,
            cache_pool: _,
            capture_group_extraction_inner: _,
            capture_groups,
        } = self;

        f.debug_struct("ReverseBackwardsDelegate")
            .field("pattern", pattern)
            .field("capture_groups", capture_groups)
            .finish()
    }
}

/// Instruction of the VM.
#[derive(Debug)]
pub enum Insn {
    /// Successful end of program
    End,
    /// Match any character (including newline)
    Any,
    /// Match any character (not including newline)
    AnyNoNL,
    /// Assertions
    Assertion(Assertion),
    /// Match the literal string at the current index
    Lit(String), // should be cow?
    /// Split execution into two threads. The two fields are positions of instructions. Execution
    /// first tries the first thread. If that fails, the second position is tried.
    Split(usize, usize),
    /// Jump to instruction at position
    Jmp(usize),
    /// Save the current string index into the specified slot
    Save(usize),
    /// Save `0` into the specified slot
    Save0(usize),
    /// Set the string index to the value that was saved in the specified slot
    Restore(usize),
    /// Repeat greedily (match as much as possible)
    RepeatGr {
        /// Minimum number of matches
        lo: usize,
        /// Maximum number of matches
        hi: usize,
        /// The instruction after the repeat
        next: usize,
        /// The slot for keeping track of the number of repetitions
        repeat: usize,
    },
    /// Repeat non-greedily (prefer matching as little as possible)
    RepeatNg {
        /// Minimum number of matches
        lo: usize,
        /// Maximum number of matches
        hi: usize,
        /// The instruction after the repeat
        next: usize,
        /// The slot for keeping track of the number of repetitions
        repeat: usize,
    },
    /// Repeat greedily and prevent infinite loops from empty matches
    RepeatEpsilonGr {
        /// Minimum number of matches
        lo: usize,
        /// The instruction after the repeat
        next: usize,
        /// The slot for keeping track of the number of repetitions
        repeat: usize,
        /// The slot for saving the previous IX to check if we had an empty match
        check: usize,
    },
    /// Repeat non-greedily and prevent infinite loops from empty matches
    RepeatEpsilonNg {
        /// Minimum number of matches
        lo: usize,
        /// The instruction after the repeat
        next: usize,
        /// The slot for keeping track of the number of repetitions
        repeat: usize,
        /// The slot for saving the previous IX to check if we had an empty match
        check: usize,
    },
    /// Negative look-around failed
    FailNegativeLookAround,
    /// Set IX back by the specified number of characters
    GoBack(usize),
    /// Back reference to a group number to check
    Backref {
        /// The save slot representing the start of the capture group
        slot: usize,
        /// Whether the backref should be matched case insensitively
        casei: bool,
    },
    /// Begin of atomic group
    BeginAtomic,
    /// End of atomic group
    EndAtomic,
    /// Delegate matching to the regex crate
    Delegate(Delegate),
    /// Anchor to match at the position where the previous match ended
    ContinueFromPreviousMatchEnd {
        /// Whether this is at the start of the pattern (allowing early exit on failure)
        at_start: bool,
    },
    /// Continue only if the specified capture group has already been populated as part of the match
    BackrefExistsCondition(usize),
    /// Immediately fail the current match attempt and trigger backtracking.
    /// This is used for backtracking control verbs like `(*FAIL)`.
    Fail,
    #[cfg(feature = "variable-lookbehinds")]
    /// Reverse lookbehind using regex-automata for variable-sized patterns
    BackwardsDelegate(ReverseBackwardsDelegate),
}

/// Sequence of instructions for the VM to execute.
#[derive(Debug)]
pub struct Prog {
    /// Instructions of the program
    pub body: Vec<Insn>,
    n_saves: usize,
}

impl Prog {
    pub(crate) fn new(body: Vec<Insn>, n_saves: usize) -> Prog {
        Prog { body, n_saves }
    }

    #[doc(hidden)]
    pub(crate) fn debug_print(&self, writer: &mut Formatter<'_>) -> core::fmt::Result {
        for (i, insn) in self.body.iter().enumerate() {
            writeln!(writer, "{:3}: {:?}", i, insn)?;
        }
        Ok(())
    }
}
