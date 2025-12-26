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

//! Helper functions for string matching and capture group management.

use crate::codepoint_len;
use regex_automata::util::primitives::NonMaxUsize;

use super::{CaptureGroupRange, State};

pub(super) fn codepoint_len_at(s: &str, ix: usize) -> usize {
    codepoint_len(s.as_bytes()[ix])
}

#[inline]
pub(super) fn matches_literal(s: &str, ix: usize, end: usize, literal: &str) -> bool {
    // Compare as bytes because the literal might be a single byte char whereas ix
    // points to a multibyte char. Comparing with str would result in an error like
    // "byte index N is not a char boundary".
    end <= s.len() && &s.as_bytes()[ix..end] == literal.as_bytes()
}

pub(super) fn matches_literal_casei(s: &str, ix: usize, end: usize, literal: &str) -> bool {
    if end > s.len() {
        return false;
    }
    if matches_literal(s, ix, end, literal) {
        return true;
    }
    if !s.is_char_boundary(ix) || !s.is_char_boundary(end) {
        return false;
    }
    if s[ix..end].is_ascii() {
        return s[ix..end].eq_ignore_ascii_case(literal);
    }

    // text captured and being backreferenced is not ascii, so we utilize regex-automata's case insensitive matching
    use regex_syntax::ast::*;
    let span = Span::splat(Position::new(0, 0, 0));
    let literals = literal
        .chars()
        .map(|c| {
            Ast::literal(Literal {
                span,
                kind: LiteralKind::Verbatim,
                c,
            })
        })
        .collect();
    let ast = Ast::concat(Concat {
        span,
        asts: literals,
    });

    let mut translator = regex_syntax::hir::translate::TranslatorBuilder::new()
        .case_insensitive(true)
        .build();
    let hir = translator.translate(literal, &ast).unwrap();

    use regex_automata::meta::Builder as RaBuilder;
    let re = RaBuilder::new()
        .build_from_hir(&hir)
        .expect("literal hir should get built successfully");
    re.find(&s[ix..end]).is_some()
}

/// Helper function to store capture group positions from inner_slots into state.
/// This is used by both Delegate and BackwardsDelegate instructions.
#[inline]
pub(super) fn store_capture_groups(
    state: &mut State,
    inner_slots: &[Option<NonMaxUsize>],
    range: CaptureGroupRange,
) {
    let start_group = range.start();
    let end_group = range.end();
    for i in 0..(end_group - start_group) {
        let slot = (start_group + i) * 2;
        if let Some(start) = inner_slots[(i + 1) * 2] {
            let end = inner_slots[(i + 1) * 2 + 1].unwrap();
            state.save(slot, start.get());
            state.save(slot + 1, end.get());
        } else {
            state.save(slot, usize::MAX);
            state.save(slot + 1, usize::MAX);
        }
    }
}
