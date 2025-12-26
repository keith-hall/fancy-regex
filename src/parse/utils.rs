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

//! Utility functions for parsing regex patterns.

use crate::Expr;
use alloc::string::String;

pub(crate) fn parse_decimal(s: &str, ix: usize) -> Option<(usize, usize)> {
    let mut end = ix;
    while end < s.len() && s.as_bytes()[end].is_ascii_digit() {
        end += 1;
    }
    s[ix..end].parse::<usize>().ok().map(|val| (end, val))
}

#[derive(Debug, PartialEq)]
pub(crate) struct ParsedId<'a> {
    pub id: &'a str,
    pub relative: Option<isize>,
    pub skip: usize,
}

/// Attempts to parse an identifier, optionally followed by a relative number between the
/// specified opening and closing delimiters.  On success, returns
/// `Some((id, relative, skip))`, where `skip` is how much of the string was used.
pub(crate) fn parse_id<'a>(
    s: &'a str,
    open: &'_ str,
    close: &'_ str,
    allow_relative: bool,
) -> Option<ParsedId<'a>> {
    debug_assert!(!close.starts_with(is_id_char));

    if !s.starts_with(open) || s.len() <= open.len() + close.len() {
        return None;
    }

    let id_start = open.len();
    let mut iter = s[id_start..].char_indices().peekable();
    let after_id = iter.find(|(_, ch)| !is_id_char(*ch));

    let id_len = match after_id.map(|(i, _)| i) {
        Some(id_len) => id_len,
        None if close.is_empty() => s.len(),
        _ => 0,
    };

    let id_end = id_start + id_len;
    if id_len > 0 && s[id_end..].starts_with(close) {
        return Some(ParsedId {
            id: &s[id_start..id_end],
            relative: None,
            skip: id_end + close.len(),
        });
    } else if !allow_relative {
        return None;
    }
    let relative_sign = s.as_bytes()[id_end];
    if relative_sign == b'+' || relative_sign == b'-' {
        if let Some((end, relative_amount)) = parse_decimal(s, id_end + 1) {
            if s[end..].starts_with(close) {
                if relative_amount == 0 && id_len == 0 {
                    return None;
                }
                let relative_amount_signed = if relative_sign == b'-' {
                    -(relative_amount as isize)
                } else {
                    relative_amount as isize
                };
                return Some(ParsedId {
                    id: &s[id_start..id_end],
                    relative: Some(relative_amount_signed),
                    skip: end + close.len(),
                });
            }
        }
    }
    None
}

pub(super) fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

pub(super) fn is_hex_digit(b: u8) -> bool {
    b.is_ascii_digit() || (b'a' <= (b | 32) && (b | 32) <= b'f')
}

pub(crate) fn make_literal(s: &str) -> Expr {
    make_literal_case_insensitive(s, false)
}

pub(crate) fn make_literal_case_insensitive(s: &str, case_insensitive: bool) -> Expr {
    Expr::Literal {
        val: String::from(s),
        casei: case_insensitive,
    }
}
