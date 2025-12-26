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

//! Atom parsing (literals, escapes, etc).

use alloc::boxed::Box;
use alloc::format;
use alloc::string::{String, ToString};

use crate::parse_flags::{FLAG_CASEI, FLAG_DOTNL, FLAG_MULTI, FLAG_ONIGURUMA_MODE};
use crate::{codepoint_len, Error, Expr, ParseError, Result};
use crate::{Assertion, LookAround::*};

use super::utils::{is_hex_digit, make_literal};
use super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn parse_atom(&mut self, ix: usize, depth: usize) -> Result<(usize, Expr)> {
        let ix = self.optional_whitespace(ix)?;
        if ix == self.re.len() {
            return Ok((ix, Expr::Empty));
        }
        match self.re.as_bytes()[ix] {
            b'.' => Ok((
                ix + 1,
                Expr::Any {
                    newline: self.flag(FLAG_DOTNL),
                },
            )),
            b'^' => Ok((
                ix + 1,
                if self.flag(FLAG_MULTI) {
                    // TODO: support crlf flag
                    Expr::Assertion(Assertion::StartLine { crlf: false })
                } else {
                    Expr::Assertion(Assertion::StartText)
                },
            )),
            b'$' => Ok((
                ix + 1,
                if self.flag(FLAG_MULTI) {
                    // TODO: support crlf flag
                    Expr::Assertion(Assertion::EndLine { crlf: false })
                } else {
                    Expr::Assertion(Assertion::EndText)
                },
            )),
            b'(' => self.parse_group(ix, depth),
            b'\\' => self.parse_escape(ix, false),
            b'+' | b'*' | b'?' | b'|' | b')' => Ok((ix, Expr::Empty)),
            b'[' => self.parse_class(ix),
            b => {
                // TODO: maybe want to match multiple codepoints?
                let next = ix + codepoint_len(b);
                Ok((
                    next,
                    Expr::Literal {
                        val: String::from(&self.re[ix..next]),
                        casei: self.flag(FLAG_CASEI),
                    },
                ))
            }
        }
    }

    // ix points to \ character
    pub(super) fn parse_escape(&mut self, ix: usize, in_class: bool) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();
        let Some(b) = bytes.get(ix + 1).copied() else {
            return Err(Error::ParseError(ix, ParseError::TrailingBackslash));
        };
        let end = ix + 1 + codepoint_len(b);
        Ok(if b.is_ascii_digit() {
            return self.parse_numbered_backref(ix + 1);
        } else if matches!(b, b'k') && !in_class {
            // Named backref: \k<name>
            if bytes.get(end) == Some(&b'\'') {
                return self.parse_named_backref(end, "'", "'", true);
            } else {
                return self.parse_named_backref(end, "<", ">", true);
            }
        } else if b == b'A' && !in_class {
            (end, Expr::Assertion(Assertion::StartText))
        } else if b == b'z' && !in_class {
            (end, Expr::Assertion(Assertion::EndText))
        } else if b == b'Z' && !in_class {
            (
                end,
                Expr::LookAround(
                    Box::new(Expr::Delegate {
                        inner: "\\n*$".to_string(),
                        size: 0,
                        casei: false,
                    }),
                    LookAhead,
                ),
            )
        } else if (b == b'b' || b == b'B') && !in_class {
            let check_pos = self.optional_whitespace(end)?;
            if bytes.get(check_pos) == Some(&b'{') {
                let next_open_brace_pos = self.optional_whitespace(check_pos + 1)?;
                let is_repetition = matches!(
                    bytes.get(next_open_brace_pos),
                    Some(&ch) if ch.is_ascii_digit() || ch == b','
                );
                if !is_repetition {
                    return self.parse_word_boundary_brace(ix);
                }
            }
            let expr = if b == b'b' {
                Expr::Assertion(Assertion::WordBoundary)
            } else {
                Expr::Assertion(Assertion::NotWordBoundary)
            };
            (end, expr)
        } else if b == b'<' && !in_class {
            let expr = if self.flag(FLAG_ONIGURUMA_MODE) {
                make_literal("<")
            } else {
                Expr::Assertion(Assertion::LeftWordBoundary)
            };
            (end, expr)
        } else if b == b'>' && !in_class {
            let expr = if self.flag(FLAG_ONIGURUMA_MODE) {
                make_literal(">")
            } else {
                Expr::Assertion(Assertion::RightWordBoundary)
            };
            (end, expr)
        } else if matches!(b | 32, b'd' | b's' | b'w') {
            (
                end,
                Expr::Delegate {
                    inner: String::from(&self.re[ix..end]),
                    size: 1,
                    casei: self.flag(FLAG_CASEI),
                },
            )
        } else if (b | 32) == b'h' {
            let s = if b == b'h' {
                "[0-9A-Fa-f]"
            } else {
                "[^0-9A-Fa-f]"
            };
            (
                end,
                Expr::Delegate {
                    inner: String::from(s),
                    size: 1,
                    casei: false,
                },
            )
        } else if b == b'x' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 2);
        } else if b == b'u' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 4);
        } else if b == b'U' {
            let end = self.optional_whitespace(end)?;
            return self.parse_hex(end, 8);
        } else if (b | 32) == b'p' && end != bytes.len() {
            let mut end = end;
            let b = bytes[end];
            end += codepoint_len(b);
            if b == b'{' {
                loop {
                    if end == self.re.len() {
                        return Err(Error::ParseError(ix, ParseError::UnclosedUnicodeName));
                    }
                    let b = bytes[end];
                    if b == b'}' {
                        end += 1;
                        break;
                    }
                    end += codepoint_len(b);
                }
            }
            (
                end,
                Expr::Delegate {
                    inner: String::from(&self.re[ix..end]),
                    size: 1,
                    casei: self.flag(FLAG_CASEI),
                },
            )
        } else if b == b'K' && !in_class {
            (end, Expr::KeepOut)
        } else if b == b'G' && !in_class {
            (end, Expr::ContinueFromPreviousMatchEnd)
        } else if b == b'O' && !in_class {
            (end, Expr::Any { newline: true })
        } else if b == b'N' && !in_class {
            (end, Expr::Any { newline: false })
        } else if b == b'g' && !in_class {
            if end == self.re.len() {
                return Err(Error::ParseError(
                    ix,
                    ParseError::InvalidEscape("\\g".to_string()),
                ));
            }
            let b = bytes[end];
            if b.is_ascii_digit() {
                self.parse_numbered_subroutine_call(end)?
            } else if b == b'\'' {
                self.parse_named_subroutine_call(end, "'", "'", true)?
            } else {
                self.parse_named_subroutine_call(end, "<", ">", true)?
            }
        } else {
            // printable ASCII (including space, see issue #29)
            (
                end,
                make_literal(match b {
                    b'a' => "\x07", // BEL
                    b'b' => "\x08", // BS
                    b'f' => "\x0c", // FF
                    b'n' => "\n",   // LF
                    b'r' => "\r",   // CR
                    b't' => "\t",   // TAB
                    b'v' => "\x0b", // VT
                    b'e' => "\x1b", // ESC
                    b' ' => " ",
                    b => {
                        let s = &self.re[ix + 1..end];
                        if b.is_ascii_alphabetic()
                            && !matches!(
                                b,
                                b'k' | b'A' | b'z' | b'b' | b'B' | b'<' | b'>' | b'K' | b'G'
                            )
                        {
                            return Err(Error::ParseError(
                                ix,
                                ParseError::InvalidEscape(format!("\\{}", s)),
                            ));
                        } else {
                            s
                        }
                    }
                }),
            )
        })
    }

    // ix points after '\x', eg to 'A0' or '{12345}', or after `\u` or `\U`
    pub(super) fn parse_hex(&self, ix: usize, digits: usize) -> Result<(usize, Expr)> {
        if ix >= self.re.len() {
            // Incomplete escape sequence
            return Err(Error::ParseError(ix, ParseError::InvalidHex));
        }
        let bytes = self.re.as_bytes();
        let b = bytes[ix];
        // Parse fixed-width hex (e.g., \xAB)
        if ix + digits <= self.re.len() && bytes[ix..ix + digits].iter().all(|&b| is_hex_digit(b)) {
            let hex_str = &self.re[ix..ix + digits];
            return self.hex_to_literal(ix, ix + digits, hex_str);
        }
        // Parse brace-enclosed hex (e.g., \u{00AB})
        if b == b'{' {
            let mut pos = ix + 1;
            let mut hex_chars = String::new();
            while pos < self.re.len() {
                // Skip whitespace/comments if FLAG_IGNORE_SPACE is set
                pos = self.optional_whitespace(pos)?;
                if pos >= self.re.len() {
                    return Err(Error::ParseError(ix, ParseError::InvalidHex));
                }
                let b = bytes[pos];
                if b == b'}' && !hex_chars.is_empty() {
                    return self.hex_to_literal(ix, pos + 1, &hex_chars);
                }
                if is_hex_digit(b) && hex_chars.len() < 8 {
                    hex_chars.push(b as char);
                    pos += 1;
                } else {
                    return Err(Error::ParseError(ix, ParseError::InvalidHex));
                }
            }
        }
        Err(Error::ParseError(ix, ParseError::InvalidHex))
    }

    pub(super) fn hex_to_literal(
        &self,
        ix: usize,
        end: usize,
        hex_str: &str,
    ) -> Result<(usize, Expr)> {
        let codepoint = u32::from_str_radix(hex_str, 16).unwrap();
        if let Some(c) = char::from_u32(codepoint) {
            Ok((
                end,
                Expr::Literal {
                    val: c.to_string(),
                    casei: self.flag(FLAG_CASEI),
                },
            ))
        } else {
            Err(Error::ParseError(ix, ParseError::InvalidCodepointValue))
        }
    }

    // ix points before '\b' or '\B'
    pub(super) fn parse_word_boundary_brace(&self, ix: usize) -> Result<(usize, Expr)> {
        let bytes = self.re.as_bytes();

        // Verify that we have '\b' or '\B'
        if !matches!(bytes.get(ix..ix + 2), Some([b'\\', b'b' | b'B'])) {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }
        // Skip whitespace/comments after \b or \B if FLAG_IGNORE_SPACE is set
        let brace_start = self.optional_whitespace(ix + 2)?;
        // Verify we have '{'
        if bytes.get(brace_start) != Some(&b'{') {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }
        // Extract content between braces
        let mut pos = brace_start + 1;
        let mut content = String::new();
        while pos < self.re.len() {
            let b = bytes[pos];
            if b == b'}' {
                break;
            }
            // Skip whitespace/comments if FLAG_IGNORE_SPACE is set
            let next_pos = self.optional_whitespace(pos)?;
            if next_pos > pos {
                // Whitespace was skipped
                pos = next_pos;
                if pos >= self.re.len() || bytes[pos] == b'}' {
                    break;
                }
            }
            // Add non-whitespace character to content
            let b = bytes[pos];
            if b != b'}' {
                content.push(b as char);
                pos += codepoint_len(b);
            }
        }

        let end_brace = pos;
        if end_brace >= self.re.len() || bytes[end_brace] != b'}' {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape("\\b{...}".to_string()),
            ));
        }

        // \B{...} is not supported
        if bytes[ix + 1] == b'B' {
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidEscape(format!("\\B{{{}}}", content)),
            ));
        }

        let expr = match content.as_str() {
            "start" => Expr::Assertion(Assertion::LeftWordBoundary),
            "end" => Expr::Assertion(Assertion::RightWordBoundary),
            "start-half" => Expr::Assertion(Assertion::LeftWordHalfBoundary),
            "end-half" => Expr::Assertion(Assertion::RightWordHalfBoundary),
            _ => {
                return Err(Error::ParseError(
                    ix,
                    ParseError::InvalidEscape(format!("\\b{{{}}}", content)),
                ));
            }
        };

        Ok((end_brace + 1, expr))
    }
}
