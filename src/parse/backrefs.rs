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

//! Backref and subroutine call parsing.

use alloc::string::ToString;

use crate::parse_flags::FLAG_CASEI;
use crate::{Error, Expr, ParseError, Result};

use super::utils::{parse_decimal, parse_id, ParsedId};
use super::Parser;

pub(super) struct NamedBackrefOrSubroutine<'a> {
    pub ix: usize,
    pub group_ix: Option<usize>,
    pub group_name: Option<&'a str>,
    pub recursion_level: Option<isize>,
}

impl<'a> Parser<'a> {
    pub(super) fn parse_named_backref(
        &mut self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<(usize, Expr)> {
        let NamedBackrefOrSubroutine {
            ix: end,
            group_ix,
            group_name,
            recursion_level,
        } = self.parse_named_backref_or_subroutine(ix, open, close, allow_relative)?;
        if let Some(group) = group_ix {
            self.backrefs.insert(group);
            return Ok((
                end,
                if let Some(recursion_level) = recursion_level {
                    Expr::BackrefWithRelativeRecursionLevel {
                        group,
                        relative_level: recursion_level,
                        casei: self.flag(FLAG_CASEI),
                    }
                } else {
                    Expr::Backref {
                        group,
                        casei: self.flag(FLAG_CASEI),
                    }
                },
            ));
        }
        if let Some(group_name) = group_name {
            // here the name was parsed but doesn't match a capture group we have already parsed
            return Err(Error::ParseError(
                ix,
                ParseError::InvalidGroupNameBackref(group_name.to_string()),
            ));
        }
        unreachable!()
    }

    pub(super) fn parse_named_subroutine_call(
        &mut self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<(usize, Expr)> {
        let NamedBackrefOrSubroutine {
            ix: end,
            group_ix,
            group_name,
            recursion_level,
        } = self.parse_named_backref_or_subroutine(ix, open, close, allow_relative)?;
        if recursion_level.is_some() {
            return Err(Error::ParseError(ix, ParseError::InvalidGroupName));
        }
        if let Some(group) = group_ix {
            self.contains_subroutines = true;
            if group == 0 {
                self.self_recursive = true;
            }
            return Ok((end, Expr::SubroutineCall(group)));
        }
        if let Some(group_name) = group_name {
            // here the name was parsed but doesn't match a capture group we have already parsed
            let expr = Expr::UnresolvedNamedSubroutineCall {
                name: group_name.to_string(),
                ix,
            };
            self.has_unresolved_subroutines = true;
            self.contains_subroutines = true;
            return Ok((end, expr));
        }
        unreachable!()
    }

    fn parse_named_backref_or_subroutine(
        &self,
        ix: usize,
        open: &str,
        close: &str,
        allow_relative: bool,
    ) -> Result<NamedBackrefOrSubroutine<'_>> {
        if let Some(ParsedId {
            id,
            mut relative,
            skip,
        }) = parse_id(&self.re[ix..], open, close, allow_relative)
        {
            let group = if let Some(group) = self.named_groups.get(id) {
                Some(*group)
            } else if let Ok(group) = id.parse::<usize>() {
                Some(group)
            } else if let Some(relative_group) = relative {
                if id.is_empty() {
                    relative = None;
                    self.curr_group.checked_add_signed(if relative_group < 0 {
                        relative_group + 1
                    } else {
                        relative_group
                    })
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(group) = group {
                Ok(NamedBackrefOrSubroutine {
                    ix: ix + skip,
                    group_ix: Some(group),
                    group_name: None,
                    recursion_level: relative,
                })
            } else {
                // here the name was parsed but doesn't match a capture group we have already parsed
                Ok(NamedBackrefOrSubroutine {
                    ix: ix + skip,
                    group_ix: None,
                    group_name: Some(id),
                    recursion_level: relative,
                })
            }
        } else {
            // in this case the name can't be parsed
            Err(Error::ParseError(ix, ParseError::InvalidGroupName))
        }
    }

    pub(super) fn parse_numbered_backref(&mut self, ix: usize) -> Result<(usize, Expr)> {
        let (end, group) = self.parse_numbered_backref_or_subroutine_call(ix)?;
        self.numeric_backrefs = true;
        self.backrefs.insert(group);
        Ok((
            end,
            Expr::Backref {
                group,
                casei: self.flag(FLAG_CASEI),
            },
        ))
    }

    pub(super) fn parse_numbered_subroutine_call(&mut self, ix: usize) -> Result<(usize, Expr)> {
        let (end, group) = self.parse_numbered_backref_or_subroutine_call(ix)?;
        self.numeric_backrefs = true;
        self.contains_subroutines = true;
        if group == 0 {
            self.self_recursive = true;
        }
        Ok((end, Expr::SubroutineCall(group)))
    }

    fn parse_numbered_backref_or_subroutine_call(&self, ix: usize) -> Result<(usize, usize)> {
        if let Some((end, group)) = parse_decimal(self.re, ix) {
            // protect BitSet against unreasonably large value
            if group < self.re.len() / 2 {
                return Ok((end, group));
            }
        }
        Err(Error::ParseError(ix, ParseError::InvalidBackref))
    }

    pub(super) fn resolve_named_subroutine_calls(&mut self, expr: &mut Expr) {
        match expr {
            Expr::UnresolvedNamedSubroutineCall { name, .. } => {
                if let Some(group) = self.named_groups.get(name) {
                    *expr = Expr::SubroutineCall(*group);
                } else {
                    self.has_unresolved_subroutines = true;
                }
            }
            // recursively resolve in inner expressions
            Expr::Group(inner) | Expr::LookAround(inner, _) | Expr::AtomicGroup(inner) => {
                self.resolve_named_subroutine_calls(inner);
            }
            Expr::Concat(children) | Expr::Alt(children) => {
                for child in children {
                    self.resolve_named_subroutine_calls(child);
                }
            }
            Expr::Repeat { child, .. } => {
                self.resolve_named_subroutine_calls(child);
            }
            Expr::Conditional {
                condition,
                true_branch,
                false_branch,
            } => {
                self.resolve_named_subroutine_calls(condition);
                self.resolve_named_subroutine_calls(true_branch);
                self.resolve_named_subroutine_calls(false_branch);
            }
            _ => {}
        }
    }
}
