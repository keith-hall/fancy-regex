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

//! AST to IR resolver.
//!
//! This module converts the raw AST into the intermediate representation (Expr).

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::{format, vec};
use bit_set::BitSet;

use crate::ast::{Ast, AstTree, AssertionKind, LookAroundKind, SubroutineTarget};
use crate::flags::*;
use crate::parse::{ExprTree, NamedGroups};
use crate::{Assertion, Expr, LookAround, Result};

/// Resolver that converts AST to Expr IR
pub struct Resolver {
    /// Backreferences found during resolution
    backrefs: BitSet,
    /// Named capture groups
    named_groups: NamedGroups,
    /// Current group number counter
    curr_group: usize,
    /// Whether subroutines are present
    contains_subroutines: bool,
    /// Whether pattern is self-recursive
    self_recursive: bool,
    /// Current flags state
    flags: u32,
}

impl Resolver {
    /// Create a new resolver with initial flags
    pub fn new(flags: u32) -> Self {
        Resolver {
            backrefs: BitSet::new(),
            named_groups: NamedGroups::new(),
            curr_group: 0,
            contains_subroutines: false,
            self_recursive: false,
            flags,
        }
    }

    /// Resolve an AST tree into an Expr tree
    pub fn resolve(ast_tree: AstTree) -> Result<ExprTree> {
        let mut resolver = Resolver::new(ast_tree.flags);
        let expr = resolver.resolve_ast(&ast_tree.ast)?;

        Ok(ExprTree {
            expr,
            backrefs: resolver.backrefs,
            named_groups: resolver.named_groups,
            contains_subroutines: resolver.contains_subroutines,
            self_recursive: resolver.self_recursive,
        })
    }

    /// Resolve a single AST node to Expr
    fn resolve_ast(&mut self, ast: &Ast) -> Result<Expr> {
        match ast {
            Ast::Empty => Ok(Expr::Empty),

            Ast::Dot { newline } => Ok(Expr::Any { newline: *newline }),

            Ast::Literal { val, casei } => Ok(Expr::Literal {
                val: val.clone(),
                casei: *casei,
            }),

            Ast::CharClass {
                content,
                negated,
                casei,
            } => {
                // Convert character class to delegate format
                let mut class_str = String::new();
                class_str.push('[');
                if *negated {
                    class_str.push('^');
                }
                class_str.push_str(content);
                class_str.push(']');

                Ok(Expr::Delegate {
                    inner: class_str,
                    size: 1, // Character classes match exactly one character
                    casei: *casei,
                })
            }

            Ast::MetaChar {
                char,
                negated,
                casei,
            } => {
                // Convert meta characters to delegate format
                let meta_str = if *negated {
                    format!("\\{}", char.to_ascii_uppercase())
                } else {
                    format!("\\{}", char)
                };

                Ok(Expr::Delegate {
                    inner: meta_str,
                    size: 1, // Meta characters match exactly one character
                    casei: *casei,
                })
            }

            Ast::Assertion(kind) => {
                let assertion = match kind {
                    AssertionKind::StartText => Assertion::StartText,
                    AssertionKind::EndText => Assertion::EndText,
                    AssertionKind::StartLine { crlf } => Assertion::StartLine { crlf: *crlf },
                    AssertionKind::EndLine { crlf } => Assertion::EndLine { crlf: *crlf },
                    AssertionKind::WordBoundary => Assertion::WordBoundary,
                    AssertionKind::NotWordBoundary => Assertion::NotWordBoundary,
                    AssertionKind::LeftWordBoundary => Assertion::LeftWordBoundary,
                    AssertionKind::RightWordBoundary => Assertion::RightWordBoundary,
                };
                Ok(Expr::Assertion(assertion))
            }

            Ast::Concat(children) => {
                if children.is_empty() {
                    return Ok(Expr::Empty);
                }
                let mut expr_children = Vec::new();
                for child in children {
                    let child_expr = self.resolve_ast(child)?;
                    if child_expr != Expr::Empty {
                        expr_children.push(child_expr);
                    }
                }
                match expr_children.len() {
                    0 => Ok(Expr::Empty),
                    1 => Ok(expr_children.into_iter().next().unwrap()),
                    _ => Ok(Expr::Concat(expr_children)),
                }
            }

            Ast::Alt(children) => {
                if children.is_empty() {
                    return Ok(Expr::Empty);
                }
                let mut expr_children = Vec::new();
                for child in children {
                    expr_children.push(self.resolve_ast(child)?);
                }
                if expr_children.len() == 1 {
                    Ok(expr_children.into_iter().next().unwrap())
                } else {
                    Ok(Expr::Alt(expr_children))
                }
            }

            Ast::Group(child) => {
                self.curr_group += 1;
                let child_expr = self.resolve_ast(child)?;
                Ok(Expr::Group(Box::new(child_expr)))
            }

            Ast::NonCapturingGroup(child) => {
                // Non-capturing groups don't increment group counter
                self.resolve_ast(child)
            }

            Ast::NamedGroup { name, expr } => {
                self.curr_group += 1;
                let group_num = self.curr_group;
                self.named_groups.insert(name.clone(), group_num);
                let child_expr = self.resolve_ast(expr)?;
                Ok(Expr::Group(Box::new(child_expr)))
            }

            Ast::AtomicGroup(child) => {
                let child_expr = self.resolve_ast(child)?;
                Ok(Expr::AtomicGroup(Box::new(child_expr)))
            }

            Ast::LookAround { expr, kind } => {
                let child_expr = self.resolve_ast(expr)?;
                let lookaround = match kind {
                    LookAroundKind::LookAhead => LookAround::LookAhead,
                    LookAroundKind::LookAheadNeg => LookAround::LookAheadNeg,
                    LookAroundKind::LookBehind => LookAround::LookBehind,
                    LookAroundKind::LookBehindNeg => LookAround::LookBehindNeg,
                };
                Ok(Expr::LookAround(Box::new(child_expr), lookaround))
            }

            Ast::Repeat {
                expr,
                min,
                max,
                greedy,
            } => {
                let child_expr = self.resolve_ast(expr)?;
                Ok(Expr::Repeat {
                    child: Box::new(child_expr),
                    lo: *min,
                    hi: *max,
                    greedy: *greedy,
                })
            }

            Ast::Backref { group, casei } => {
                self.backrefs.insert(*group);
                Ok(Expr::Backref {
                    group: *group,
                    casei: *casei,
                })
            }

            Ast::NamedBackref { name, casei } => {
                // For now, we'll need to resolve named backrefs later
                // when we have all named groups collected
                // This is a simplified implementation
                if let Some(&group_num) = self.named_groups.get(name) {
                    self.backrefs.insert(group_num);
                    Ok(Expr::Backref {
                        group: group_num,
                        casei: *casei,
                    })
                } else {
                    // Group not yet defined, this may be handled later
                    // For now, create a placeholder
                    Ok(Expr::Backref {
                        group: 0, // Invalid group, will be caught by analyzer
                        casei: *casei,
                    })
                }
            }

            Ast::RelativeBackref {
                group,
                relative_level,
                casei,
            } => {
                self.backrefs.insert(*group);
                Ok(Expr::BackrefWithRelativeRecursionLevel {
                    group: *group,
                    relative_level: *relative_level,
                    casei: *casei,
                })
            }

            Ast::Flags { flags: _, expr } => {
                // For now, we'll ignore flag changes and just process the expression
                // A more complete implementation would track flag state changes
                match expr {
                    Some(child) => self.resolve_ast(child),
                    None => Ok(Expr::Empty),
                }
            }

            Ast::Comment { text: _ } => {
                // Comments are ignored in the IR
                Ok(Expr::Empty)
            }

            Ast::Conditional {
                condition,
                true_branch,
                false_branch,
            } => {
                let cond_expr = self.resolve_ast(condition)?;
                let true_expr = self.resolve_ast(true_branch)?;
                let false_expr = self.resolve_ast(false_branch)?;

                Ok(Expr::Conditional {
                    condition: Box::new(cond_expr),
                    true_branch: Box::new(true_expr),
                    false_branch: Box::new(false_expr),
                })
            }

            Ast::SubroutineCall { target } => {
                self.contains_subroutines = true;
                match target {
                    SubroutineTarget::Recursive => {
                        self.self_recursive = true;
                        Ok(Expr::SubroutineCall(0))
                    }
                    SubroutineTarget::Group(group) => Ok(Expr::SubroutineCall(*group)),
                    SubroutineTarget::Named(name) => {
                        // This will need to be resolved later when all groups are known
                        Ok(Expr::UnresolvedNamedSubroutineCall {
                            name: name.clone(),
                            ix: 0, // We don't have position info in AST
                        })
                    }
                    SubroutineTarget::Relative(_) => {
                        // Relative subroutine calls need special handling
                        // For now, treat as regular subroutine call
                        Ok(Expr::SubroutineCall(0))
                    }
                }
            }

            Ast::KeepOut => Ok(Expr::KeepOut),

            Ast::ContinueFromPreviousMatchEnd => Ok(Expr::ContinueFromPreviousMatchEnd),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_resolve_literal() {
        let ast = Ast::Literal {
            val: "hello".to_string(),
            casei: false,
        };
        let ast_tree = AstTree { ast, flags: 0 };
        let expr_tree = Resolver::resolve(ast_tree).unwrap();
        
        assert_eq!(
            expr_tree.expr,
            Expr::Literal {
                val: "hello".to_string(),
                casei: false,
            }
        );
    }

    #[test]
    fn test_resolve_char_class() {
        let ast = Ast::CharClass {
            content: "a-z".to_string(),
            negated: false,
            casei: false,
        };
        let ast_tree = AstTree { ast, flags: 0 };
        let expr_tree = Resolver::resolve(ast_tree).unwrap();
        
        if let Expr::Delegate { inner, size, casei } = expr_tree.expr {
            assert_eq!(inner, "[a-z]");
            assert_eq!(size, 1);
            assert!(!casei);
        } else {
            panic!("Expected Delegate expression");
        }
    }

    #[test]
    fn test_resolve_meta_char() {
        let ast = Ast::MetaChar {
            char: 'w',
            negated: false,
            casei: false,
        };
        let ast_tree = AstTree { ast, flags: 0 };
        let expr_tree = Resolver::resolve(ast_tree).unwrap();
        
        if let Expr::Delegate { inner, size, casei } = expr_tree.expr {
            assert_eq!(inner, "\\w");
            assert_eq!(size, 1);
            assert!(!casei);
        } else {
            panic!("Expected Delegate expression");
        }
    }
}