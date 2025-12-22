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

//! Iterator over ExprTree for easier traversal without visitor pattern pitfalls.

use alloc::vec::Vec;
use core::slice::Iter;

use crate::Expr;

/// An item yielded by the ExprTree iterator, containing the current expression
/// and its parent stack (from root to immediate parent).
#[derive(Debug, Clone)]
pub struct ExprItem<'a> {
    /// The current expression node
    pub expr: &'a Expr,
    /// Stack of parent expressions, with root at index 0 and immediate parent at the end
    pub parents: Vec<&'a Expr>,
}
    /// The current expression node
    pub expr: &'a Expr,
    /// Stack of parent expressions, with root at index 0 and immediate parent at the end
    pub parents: Vec<&'a Expr>,
}

/// Iterator over all expressions in an ExprTree, yielding each node with its parent context.
/// Uses iterative stack-based traversal to avoid recursion depth limits and minimize overhead.
pub struct ExprIter<'a> {
    stack: Vec<Iter<'a, Expr>>,
    parents: Vec<&'a Expr>,
}

impl<'a> ExprIter<'a> {
    /// Create a new iterator over the given expression tree.
    pub fn new(expr: &'a Expr) -> Self {
    stack: Vec<Iter<'a, Expr>>,
    parents: Vec<&'a Expr>,
}

impl<'a> ExprIter<'a> {
    /// Create a new iterator over the given expression tree.
    pub fn new(expr: &'a Expr) -> Self {
        let mut stack = Vec::new();
        let mut parents = Vec::new();

        // Push the root expression
        stack.push([expr].iter());
        parents.push(expr);

        ExprIter { stack, parents }
    }

    /// Get the children of an expression as a slice for iteration.
    fn get_children(expr: &Expr) -> &[Expr] {
        match expr {
            Expr::Concat(children) | Expr::Alt(children) => children,
            Expr::Group(child) | Expr::LookAround(child, _) | Expr::AtomicGroup(child) => core::slice::from_ref(child),
            Expr::Repeat { child, .. } => core::slice::from_ref(child),
            Expr::Conditional { condition, true_branch, false_branch } => {
                // For conditional, we handle children specially in next() to avoid allocation
                &[]
            }
            _ => &[],
        }
    }

    /// Handle special cases like Conditional that have multiple children
    fn push_special_children(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Conditional { condition, true_branch, false_branch } => {
                // Push children in order: condition, true_branch, false_branch
                // Create a vec to hold the references
                let children = vec![condition, true_branch, false_branch];
                self.stack.push(children.iter());
                self.parents.push(expr);
            }
            _ => {}
        }
    }
}

impl<'a> Iterator for ExprIter<'a> {
    type Item = ExprItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(iter) = self.stack.last_mut() {
            if let Some(expr) = iter.next() {
                // Yield the current expression
                let item = ExprItem {
                    expr,
                    parents: self.parents.clone(),
                };

                // Push children onto stack if any
                let children = Self::get_children(expr);
                if !children.is_empty() {
                    self.stack.push(children.iter());
                    self.parents.push(expr);
                } else {
                    // Handle special cases with multiple children
                    self.push_special_children(expr);
                }

                return Some(item);
            } else {
                // No more children at this level, pop the stack
                self.stack.pop();
                self.parents.pop();
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Expr;

    #[test]
    fn test_simple_literal() {
        let expr = Expr::Literal {
            val: "hello".to_string(),
            casei: false,
        };
        let mut iter = ExprIter::new(&expr);
        let item = iter.next().unwrap();
        assert!(matches!(item.expr, Expr::Literal { .. }));
        assert_eq!(item.parents.len(), 1);
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_concat() {
        let expr = Expr::Concat(vec![
            Expr::Literal { val: "a".to_string(), casei: false },
            Expr::Literal { val: "b".to_string(), casei: false },
        ]);
        let mut iter = ExprIter::new(&expr);

        // Root concat
        let item = iter.next().unwrap();
        assert!(matches!(item.expr, Expr::Concat(_)));
        assert_eq!(item.parents.len(), 1);

        // First literal
        let item = iter.next().unwrap();
        assert!(matches!(item.expr, Expr::Literal { .. }));
        assert_eq!(item.parents.len(), 2);
        assert!(matches!(item.parents[1], Expr::Concat(_)));

        // Second literal
        let item = iter.next().unwrap();
        assert!(matches!(item.expr, Expr::Literal { .. }));
        assert_eq!(item.parents.len(), 2);

        assert!(iter.next().is_none());
    }
}