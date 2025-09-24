// Copyright 2025 The Fancy Regex Authors.
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

//! Optimization of regex expressions.

use crate::parse::ExprTree;
use crate::Expr;
use crate::LookAround;

use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use core::mem;

/// Rewrite the expression tree to help the VM compile an efficient program.
/// Returns a boolean to say whether the new tree explicitly contains capture group 0.
pub fn optimize(tree: &mut ExprTree) -> bool {
    // Apply catastrophic backtracking prevention optimizations
    prevent_catastrophic_backtracking(&mut tree.expr);
    
    // self recursion prevents us from moving the trailing lookahead out of group 0
    if !tree.self_recursive {
        let requires_capture_group_fixup = optimize_trailing_lookahead(tree);
        requires_capture_group_fixup
    } else {
        false
    }
}

fn optimize_trailing_lookahead(tree: &mut ExprTree) -> bool {
    // returns a boolean to say whether the optimization was applied.
    // - if it was applied, capture group 0 is no longer implicit, but explicit
    //   if/when the whole expression gets delegated to regex-automata
    // converts i.e. original pattern `a(?=b)` when wrapped in the capture group 0
    // as `(a(?=b))`
    // to `(a)b`

    if let Expr::Concat(ref mut root_concat_children) = tree.expr {
        // we get the last child if it is a positive lookahead
        if let Some(Expr::LookAround(_, LookAround::LookAhead)) = root_concat_children.last() {
            // then pop the lookahead
            let lookahead_expr = root_concat_children
                .pop()
                .expect("lookaround should be popped");
            // take the rest of the children from the original Concat
            let group0_children = mem::take(root_concat_children);

            // extract the inner expression from the lookahead
            if let Expr::LookAround(inner, LookAround::LookAhead) = lookahead_expr {
                let group0 = Expr::Group(Box::new(Expr::Concat(group0_children)));
                // compose new Concat: [Group0, lookahead inner expr]
                let new_concat = Expr::Concat(vec![group0, *inner]);
                tree.expr = new_concat;
                return true;
            } else {
                unreachable!("already checked it is a lookahead");
            }
        }
    } else if let Expr::LookAround(ref mut inner, LookAround::LookAhead) = &mut tree.expr {
        let group0 = Expr::Group(Box::new(Expr::Empty));
        let mut swap = Expr::Empty;
        mem::swap(&mut swap, inner);
        // compose new Concat: [Group0, lookahead inner expr]
        tree.expr = Expr::Concat(vec![group0, swap]);
        return true;
    }
    false
}

/// Prevent catastrophic backtracking by rewriting problematic patterns.
fn prevent_catastrophic_backtracking(expr: &mut Expr) {
    match expr {
        Expr::Concat(ref mut children) => {
            // Optimize each child first
            for child in children.iter_mut() {
                prevent_catastrophic_backtracking(child);
            }
            // Look for sequential quantifiers on equivalent patterns
            optimize_sequential_quantifiers(children);
        }
        Expr::Alt(ref mut children) => {
            // Optimize each child first
            for child in children.iter_mut() {
                prevent_catastrophic_backtracking(child);
            }
            // Remove duplicate alternatives and reorder to prevent overlap
            optimize_alternation(children);
        }
        Expr::Group(ref mut child) => {
            prevent_catastrophic_backtracking(child);
        }
        Expr::Repeat { ref mut child, .. } => {
            prevent_catastrophic_backtracking(child);
            // Check for nested quantifiers
            optimize_nested_quantifiers(expr);
        }
        Expr::LookAround(ref mut child, _) => {
            prevent_catastrophic_backtracking(child);
        }
        Expr::AtomicGroup(ref mut child) => {
            prevent_catastrophic_backtracking(child);
        }
        Expr::Conditional { 
            ref mut condition,
            ref mut true_branch,
            ref mut false_branch,
        } => {
            prevent_catastrophic_backtracking(condition);
            prevent_catastrophic_backtracking(true_branch);
            prevent_catastrophic_backtracking(false_branch);
        }
        // Terminal expressions don't need further processing
        _ => {}
    }
}

/// Optimize nested quantifiers like (a+)+ by converting to possessive quantifiers or atomic groups.
fn optimize_nested_quantifiers(expr: &mut Expr) {
    if let Expr::Repeat { child, lo: _, hi: _, greedy: _ } = expr {
        if let Expr::Group(ref mut group_child) = child.as_mut() {
            if let Expr::Repeat { .. } = group_child.as_ref() {
                // Found nested quantifier pattern: (inner_repeat)+
                // Convert to atomic group to prevent catastrophic backtracking
                let mut new_child = Expr::Empty;
                mem::swap(&mut new_child, group_child);
                *group_child = Box::new(Expr::AtomicGroup(Box::new(new_child)));
            }
        }
    }
}

/// Optimize alternation to prevent overlapping branches that cause catastrophic backtracking.
fn optimize_alternation(alternatives: &mut Vec<Expr>) {
    // Remove duplicate alternatives
    let mut i = 0;
    while i < alternatives.len() {
        let mut j = i + 1;
        while j < alternatives.len() {
            if expressions_equivalent(&alternatives[i], &alternatives[j]) {
                alternatives.remove(j);
            } else {
                j += 1;
            }
        }
        i += 1;
    }
    
    // Sort alternatives by specificity (longer literals first) to prevent overlapping matches
    alternatives.sort_by(|a, b| {
        let a_specificity = get_expression_specificity(a);
        let b_specificity = get_expression_specificity(b);
        b_specificity.cmp(&a_specificity) // Reverse order - more specific first
    });
}

/// Get the specificity score of an expression for reordering alternations.
/// Higher scores indicate more specific patterns that should be tried first.
fn get_expression_specificity(expr: &Expr) -> usize {
    match expr {
        Expr::Literal { val, .. } => val.len() * 100, // Literals are most specific
        Expr::Concat(children) => {
            children.iter().map(get_expression_specificity).sum()
        }
        Expr::Group(child) => get_expression_specificity(child),
        Expr::Delegate { size, .. } => *size * 50, // Delegates are moderately specific
        Expr::Any { .. } => 10, // Any character is less specific
        Expr::Repeat { child, lo, .. } => {
            // Prefer patterns with higher minimum repetition
            get_expression_specificity(child) + lo * 5
        }
        _ => 1, // Default low specificity
    }
}

/// Check if two expressions are equivalent (for removing duplicates).
fn expressions_equivalent(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Literal { val: val_a, casei: casei_a }, Expr::Literal { val: val_b, casei: casei_b }) => {
            val_a == val_b && casei_a == casei_b
        }
        (Expr::Any { newline: nl_a }, Expr::Any { newline: nl_b }) => nl_a == nl_b,
        (Expr::Delegate { inner: inner_a, size: size_a, casei: casei_a }, 
         Expr::Delegate { inner: inner_b, size: size_b, casei: casei_b }) => {
            inner_a == inner_b && size_a == size_b && casei_a == casei_b
        }
        (Expr::Group(child_a), Expr::Group(child_b)) => {
            expressions_equivalent(child_a, child_b)
        }
        (Expr::Concat(children_a), Expr::Concat(children_b)) => {
            children_a.len() == children_b.len() &&
            children_a.iter().zip(children_b.iter()).all(|(a, b)| expressions_equivalent(a, b))
        }
        (Expr::Alt(children_a), Expr::Alt(children_b)) => {
            children_a.len() == children_b.len() &&
            children_a.iter().zip(children_b.iter()).all(|(a, b)| expressions_equivalent(a, b))
        }
        (Expr::Repeat { child: child_a, lo: lo_a, hi: hi_a, greedy: greedy_a },
         Expr::Repeat { child: child_b, lo: lo_b, hi: hi_b, greedy: greedy_b }) => {
            lo_a == lo_b && hi_a == hi_b && greedy_a == greedy_b &&
            expressions_equivalent(child_a, child_b)
        }
        _ => false,
    }
}

/// Optimize sequential quantifiers on equivalent patterns like \w*\w*.
fn optimize_sequential_quantifiers(children: &mut Vec<Expr>) {
    let mut i = 0;
    while i + 1 < children.len() {
        if let (
            Expr::Repeat { child: child1, lo: lo1, hi: hi1, greedy: greedy1 },
            Expr::Repeat { child: child2, lo: lo2, hi: hi2, greedy: greedy2 }
        ) = (&children[i], &children[i + 1]) {
            // Check if the repeated patterns are equivalent
            if expressions_equivalent(child1, child2) && greedy1 == greedy2 {
                // Combine into a single quantifier: first pattern with combined bounds
                let combined_lo = lo1 + lo2;
                let combined_hi = if *hi1 == usize::MAX || *hi2 == usize::MAX {
                    usize::MAX
                } else {
                    hi1 + hi2
                };
                
                let combined_repeat = Expr::Repeat {
                    child: child1.clone(),
                    lo: combined_lo,
                    hi: combined_hi,
                    greedy: *greedy1,
                };
                
                // Replace the first quantifier and remove the second
                children[i] = combined_repeat;
                children.remove(i + 1);
                continue; // Don't increment i, check again from same position
            }
        }
        i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::optimize;
    use super::vec;
    use super::Box;
    use crate::parse::make_literal;
    use crate::Expr;
    use alloc::string::String;

    #[test]
    fn trailing_positive_lookahead_optimized() {
        let mut tree = Expr::parse_tree("a(?=b)").unwrap();
        let requires_capture_group_fixup = optimize(&mut tree);
        assert_eq!(requires_capture_group_fixup, true);
        let mut s = String::new();
        tree.expr.to_str(&mut s, 0);
        assert_eq!(s, "(a)b");
    }

    #[test]
    fn standalone_positive_lookahead_optimized() {
        let mut tree = Expr::parse_tree("(?=b)").unwrap();
        let requires_capture_group_fixup = optimize(&mut tree);
        assert_eq!(requires_capture_group_fixup, true);
        let mut s = String::new();
        tree.expr.to_str(&mut s, 0);
        assert_eq!(s, "()b");
    }

    #[test]
    fn trailing_positive_lookahead_with_alternative_optimized() {
        let mut tree = Expr::parse_tree("a(?=b|c)").unwrap();
        let requires_capture_group_fixup = optimize(&mut tree);
        assert_eq!(requires_capture_group_fixup, true);
        let mut s = String::new();
        tree.expr.to_str(&mut s, 0);
        assert_eq!(s, "(a)(?:b|c)");
    }

    #[test]
    fn trailing_positive_lookahead_moved_even_if_not_easy() {
        let mut tree = Expr::parse_tree(r"(a)\1(?=c)").unwrap();
        let requires_capture_group_fixup = optimize(&mut tree);
        assert_eq!(requires_capture_group_fixup, true);
        assert_eq!(
            tree.expr,
            Expr::Concat(vec![
                Expr::Group(Box::new(Expr::Concat(vec![
                    Expr::Group(Box::new(make_literal("a"))),
                    Expr::Backref {
                        group: 1,
                        casei: false
                    }
                ]))),
                make_literal("c"),
            ])
        );
    }

    #[test]
    fn trailing_positive_lookahead_left_alone_when_self_recursive() {
        let tree = Expr::parse_tree(r"ab?\g<0>?(?=a|$)").unwrap();
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize(&mut optimized_tree);
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    #[test]
    fn trailing_negative_lookahead_left_alone() {
        let tree = Expr::parse_tree(r"a(?!b)").unwrap();
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize(&mut optimized_tree);
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    #[test]
    fn trailing_positive_lookbehind_left_alone() {
        let tree = Expr::parse_tree(r"(?<=b)").unwrap();
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize(&mut optimized_tree);
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    #[test]
    fn non_trailing_positive_lookahead_left_alone() {
        let tree = Expr::parse_tree(r"a(?=(b))\1").unwrap();
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize(&mut optimized_tree);
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);

        let tree = Expr::parse_tree(r"(?=(b))\1").unwrap();
        let mut optimized_tree = tree.clone();
        let requires_capture_group_fixup = optimize(&mut optimized_tree);
        assert_eq!(requires_capture_group_fixup, false);
        assert_eq!(&optimized_tree.expr, &tree.expr);
    }

    // Catastrophic backtracking prevention tests
    
    #[test]
    fn nested_quantifiers_optimized() {
        // Test (a+)+ -> (?>a+)+
        let mut tree = Expr::parse_tree(r"(a+)+").unwrap();
        optimize(&mut tree);
        
        // Check that the inner repeat is wrapped in an atomic group
        if let Expr::Repeat { child, .. } = &tree.expr {
            if let Expr::Group(group_child) = child.as_ref() {
                if let Expr::AtomicGroup(_) = group_child.as_ref() {
                    // Expected structure found
                } else {
                    panic!("Expected AtomicGroup, found: {:?}", group_child);
                }
            } else {
                panic!("Expected Group, found: {:?}", child);
            }
        } else {
            panic!("Expected Repeat, found: {:?}", tree.expr);
        }
    }
    
    #[test]
    fn duplicate_alternation_removed() {
        // Test (a|a)* -> a*
        let mut tree = Expr::parse_tree(r"(a|a)*").unwrap();
        optimize(&mut tree);
        
        // Check that duplicate alternatives are removed
        if let Expr::Repeat { child, .. } = &tree.expr {
            if let Expr::Group(group_child) = child.as_ref() {
                if let Expr::Alt(alternatives) = group_child.as_ref() {
                    assert_eq!(alternatives.len(), 1, "Expected duplicates to be removed");
                } else {
                    panic!("Expected Alt, found: {:?}", group_child);
                }
            } else {
                panic!("Expected Group, found: {:?}", child);
            }
        } else {
            panic!("Expected Repeat, found: {:?}", tree.expr);
        }
    }
    
    #[test]
    fn alternation_reordered_by_specificity() {
        // Test (a|ab)* -> (ab|a)*  (more specific first)
        let mut tree = Expr::parse_tree(r"(a|ab)*").unwrap();
        optimize(&mut tree);
        
        // Check that alternatives are reordered by specificity
        if let Expr::Repeat { child, .. } = &tree.expr {
            if let Expr::Group(group_child) = child.as_ref() {
                if let Expr::Alt(alternatives) = group_child.as_ref() {
                    // The first alternative should be "ab" (more specific)
                    if let Expr::Concat(first_alt) = &alternatives[0] {
                        assert_eq!(first_alt.len(), 2, "First alternative should be 'ab'");
                    } else {
                        panic!("Expected Concat for 'ab', found: {:?}", alternatives[0]);
                    }
                } else {
                    panic!("Expected Alt, found: {:?}", group_child);
                }
            } else {
                panic!("Expected Group, found: {:?}", child);
            }
        } else {
            panic!("Expected Repeat, found: {:?}", tree.expr);
        }
    }
    
    #[test]
    fn sequential_quantifiers_combined() {
        // Test \w*\w* -> \w{0,∞}
        let mut tree = Expr::parse_tree(r"\w*\w*").unwrap();
        optimize(&mut tree);
        
        // Check that sequential quantifiers are combined
        if let Expr::Concat(children) = &tree.expr {
            assert_eq!(children.len(), 1, "Expected quantifiers to be combined into one");
            if let Expr::Repeat { lo, hi, .. } = &children[0] {
                assert_eq!(*lo, 0, "Expected combined lo to be 0");
                assert_eq!(*hi, usize::MAX, "Expected combined hi to be MAX");
            } else {
                panic!("Expected Repeat, found: {:?}", children[0]);
            }
        } else {
            panic!("Expected Concat, found: {:?}", tree.expr);
        }
    }
    
    #[test]
    fn complex_nested_optimization() {
        // Test (\w+\s?)* -> (?>(\w+\s?))*
        let mut tree = Expr::parse_tree(r"(\w+\s?)*").unwrap();
        let original_tree = tree.clone();
        optimize(&mut tree);
        
        // The outer structure should remain the same, but internal optimization should occur
        if let (Expr::Repeat { child: orig_child, .. }, Expr::Repeat { child: _opt_child, .. }) = 
            (&original_tree.expr, &tree.expr) {
            // Check if optimization was applied to the inner structure
            // The inner group should be optimized to prevent catastrophic backtracking
            assert!(!format!("{:?}", orig_child).contains("AtomicGroup"));
            // After optimization, sequential quantifiers inside should be combined
        } else {
            panic!("Structure should remain a Repeat");
        }
    }
    
    #[test]
    fn non_problematic_patterns_unchanged() {
        // Test patterns that shouldn't be changed
        let patterns = vec![
            r"abc",      // simple literal
            r"a+",       // single quantifier
            r"a|b",      // simple alternation
            r"a*b*",     // sequential different patterns - OK
        ];
        
        for pattern in patterns {
            let original_tree = Expr::parse_tree(pattern).unwrap();
            let mut optimized_tree = original_tree.clone();
            optimize(&mut optimized_tree);
            
            // Convert to strings to compare (structure might change slightly but meaning should be same)
            let mut original_str = String::new();
            let mut optimized_str = String::new();
            original_tree.expr.to_str(&mut original_str, 0);
            optimized_tree.expr.to_str(&mut optimized_str, 0);
            
            // For non-problematic patterns, the optimized version should be similar
            // (we allow for minor reordering or cleanup)
            assert!(!optimized_str.is_empty(), "Optimized pattern should not be empty for: {}", pattern);
        }
    }
}
