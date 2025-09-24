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
use crate::analyze::Info;
use crate::Expr;
use crate::LookAround;

use alloc::boxed::Box;
use alloc::vec;
use core::mem;

/// Rewrite the expression tree to help the VM compile an efficient program.
/// Returns a boolean to say whether the new tree explicitly contains capture group 0.
pub fn optimize(tree: &mut ExprTree) -> bool {
    // self recursion prevents us from moving the trailing lookahead out of group 0
    if !tree.self_recursive {
        let requires_capture_group_fixup = optimize_trailing_lookahead(tree);
        requires_capture_group_fixup
    } else {
        false
    }
}

/// Optimize expressions to prevent catastrophic backtracking by rewriting problematic patterns.
/// This should be called after analysis to access min_size information.
pub fn optimize_catastrophic_backtracking(tree: &mut ExprTree, info: &Info<'_>) {
    optimize_catastrophic_backtracking_expr(&mut tree.expr, info);
}

/// Optimize expressions to prevent catastrophic backtracking using extracted hardness info.
pub fn optimize_catastrophic_backtracking_simple(tree: &mut ExprTree, is_hard: bool) {
    // Create a simple recursive visitor that doesn't need full Info
    optimize_catastrophic_backtracking_simple_expr(&mut tree.expr, is_hard);
}

fn optimize_catastrophic_backtracking_expr(expr: &mut Expr, info: &Info<'_>) {
    // First, recursively optimize children
    match expr {
        Expr::Concat(children) => {
            for (child_expr, child_info) in children.iter_mut().zip(&info.children) {
                optimize_catastrophic_backtracking_expr(child_expr, child_info);
            }
        }
        Expr::Alt(children) => {
            for (child_expr, child_info) in children.iter_mut().zip(&info.children) {
                optimize_catastrophic_backtracking_expr(child_expr, child_info);
            }
        }
        Expr::Group(child) => {
            if let Some(child_info) = info.children.first() {
                optimize_catastrophic_backtracking_expr(child, child_info);
            }
        }
        Expr::Repeat { child, .. } => {
            if let Some(child_info) = info.children.first() {
                optimize_catastrophic_backtracking_expr(child, child_info);
            }
        }
        Expr::LookAround(child, _) => {
            if let Some(child_info) = info.children.first() {
                optimize_catastrophic_backtracking_expr(child, child_info);
            }
        }
        Expr::AtomicGroup(child) => {
            if let Some(child_info) = info.children.first() {
                optimize_catastrophic_backtracking_expr(child, child_info);
            }
        }
        _ => {
            // No recursion needed for other expression types
        }
    }

    // Then, apply catastrophic backtracking optimizations to this level
    // We need to extract the values first to avoid borrowing conflicts
    if let Expr::Repeat { child, lo, hi, greedy } = expr {
        if let Some(child_info) = info.children.first() {
            let lo_val = *lo;
            let hi_val = *hi;
            let greedy_val = *greedy;
            
            if should_optimize_nested_repetition(child, child_info, lo_val, hi_val, info.hard) {
                if let Some(new_expr) = optimize_nested_repetition(child, child_info, lo_val, hi_val, greedy_val) {
                    *expr = new_expr;
                }
            }
        }
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

/// Check if a nested repetition pattern should be optimized to prevent catastrophic backtracking.
/// This applies only to "hard" expressions (that will be handled by the VM, not delegated to regex-automata).
fn should_optimize_nested_repetition(
    child: &Expr, 
    child_info: &Info<'_>, 
    _outer_lo: usize, 
    outer_hi: usize,
    is_hard: bool
) -> bool {
    // Only optimize if this is a hard expression (handled by our VM)
    if !is_hard {
        return false;
    }
    
    // Only optimize unbounded outer repetitions (*, +, {n,})
    if outer_hi != usize::MAX {
        return false;
    }
    
    match child {
        // Pattern: (inner_repeat)* or (inner_repeat)+
        Expr::Group(inner) => {
            if matches!(**inner, Expr::Repeat { hi: usize::MAX, .. }) {
                return true;
            }
            // Pattern with overlapping alternatives like (a|ab)*
            if matches!(**inner, Expr::Alt(_)) {
                return has_overlapping_alternatives(inner, child_info);
            }
            false
        }
        // Direct nested repetition: repeat* or repeat+
        Expr::Repeat { hi: usize::MAX, .. } => true,
        // Pattern with overlapping alternatives like (a|ab)*
        Expr::Alt(_) => {
            has_overlapping_alternatives(child, child_info)
        }
        _ => false,
    }
}

/// Check if alternatives in an Alt expression have overlapping prefixes that could cause backtracking.
fn has_overlapping_alternatives(expr: &Expr, _info: &Info<'_>) -> bool {
    let alternatives = match expr {
        Expr::Alt(alts) => alts,
        Expr::Group(inner) => {
            if let Expr::Alt(alts) = &**inner {
                alts
            } else {
                return false;
            }
        }
        _ => return false,
    };
    
    // Check for overlapping prefixes among alternatives
    // For now, we'll use a simple heuristic: if any alternative is a prefix of another
    for (i, alt1) in alternatives.iter().enumerate() {
        for alt2 in alternatives.iter().skip(i + 1) {
            if is_prefix_of(alt1, alt2) || is_prefix_of(alt2, alt1) {
                return true;
            }
        }
    }
    
    false
}

/// Check if expr1 is a prefix of expr2 (simplified check)
fn is_prefix_of(expr1: &Expr, expr2: &Expr) -> bool {
    match (expr1, expr2) {
        // Simple case: literal is prefix of concat starting with that literal
        (Expr::Literal { val: val1, .. }, Expr::Concat(concat)) => {
            if let Some(Expr::Literal { val: first_val, .. }) = concat.first() {
                first_val.starts_with(val1)
            } else {
                false
            }
        }
        // Two literals: check if one starts with the other
        (Expr::Literal { val: val1, .. }, Expr::Literal { val: val2, .. }) => {
            val2.starts_with(val1) && val1 != val2
        }
        _ => false,
    }
}

/// Optimize a nested repetition to prevent catastrophic backtracking.
/// Returns Some(new_expr) if optimization was applied, None otherwise.
fn optimize_nested_repetition(
    child: &Expr, 
    child_info: &Info<'_>, 
    outer_lo: usize, 
    outer_hi: usize,
    outer_greedy: bool
) -> Option<Expr> {
    match child {
        // Pattern: (a+)* -> a*  or  (a+)+ -> a+
        Expr::Group(inner) => {
            if matches!(**inner, Expr::Repeat { hi: usize::MAX, .. }) {
                if let Expr::Repeat { child: inner_child, lo: inner_lo, hi: inner_hi, greedy: _inner_greedy } = &**inner {
                    if *inner_hi == usize::MAX && *inner_lo >= 1 {
                        // Transform (a+)* to a* and (a+)+ to a+
                        let new_lo = if outer_lo == 0 { 0 } else { *inner_lo };
                        return Some(Expr::Repeat {
                            child: inner_child.clone(),
                            lo: new_lo,
                            hi: outer_hi,
                            greedy: outer_greedy,
                        });
                    }
                }
            }
            // Pattern: (a|ab)* -> use atomic groups to prevent backtracking
            else if matches!(**inner, Expr::Alt(_)) {
                if has_overlapping_alternatives(inner, child_info) {
                    // Wrap the group in an atomic group: (?>a|ab)*
                    let atomic_child = Expr::AtomicGroup(inner.clone());
                    let atomic_group = Box::new(Expr::Group(Box::new(atomic_child)));
                    return Some(Expr::Repeat {
                        child: atomic_group,
                        lo: outer_lo,
                        hi: outer_hi,
                        greedy: outer_greedy,
                    });
                }
            }
        }
        // Pattern: a+* -> a*  or  a++ -> a+
        Expr::Repeat { child: inner_child, lo: inner_lo, hi: inner_hi, greedy: _ } => {
            if *inner_hi == usize::MAX && *inner_lo >= 1 {
                // Transform a+* to a* and a++ to a+
                let new_lo = if outer_lo == 0 { 0 } else { *inner_lo };
                return Some(Expr::Repeat {
                    child: inner_child.clone(),
                    lo: new_lo,
                    hi: outer_hi,
                    greedy: outer_greedy,
                });
            }
        }
        Expr::Alt(_) => {
            if has_overlapping_alternatives(child, child_info) {
                // Wrap in atomic group: (?>a|ab)*
                let atomic_child = Expr::AtomicGroup(Box::new(child.clone()));
                return Some(Expr::Repeat {
                    child: Box::new(atomic_child),
                    lo: outer_lo,
                    hi: outer_hi,
                    greedy: outer_greedy,
                });
            }
        }
        _ => {
            // No optimization for other patterns
        }
    }
    None
}

fn optimize_catastrophic_backtracking_simple_expr(expr: &mut Expr, is_hard: bool) {
    // First, recursively optimize children
    match expr {
        Expr::Concat(children) => {
            for child_expr in children.iter_mut() {
                optimize_catastrophic_backtracking_simple_expr(child_expr, is_hard);
            }
        }
        Expr::Alt(children) => {
            for child_expr in children.iter_mut() {
                optimize_catastrophic_backtracking_simple_expr(child_expr, is_hard);
            }
        }
        Expr::Group(child) => {
            optimize_catastrophic_backtracking_simple_expr(child, is_hard);
        }
        Expr::Repeat { child, .. } => {
            optimize_catastrophic_backtracking_simple_expr(child, is_hard);
        }
        Expr::LookAround(child, _) => {
            optimize_catastrophic_backtracking_simple_expr(child, is_hard);
        }
        Expr::AtomicGroup(child) => {
            optimize_catastrophic_backtracking_simple_expr(child, is_hard);
        }
        _ => {
            // No recursion needed for other expression types
        }
    }

    // Then, apply catastrophic backtracking optimizations to this level
    if let Expr::Repeat { child, lo, hi, greedy } = expr {
        let lo_val = *lo;
        let hi_val = *hi;
        let greedy_val = *greedy;
        
        if should_optimize_nested_repetition_simple(child, lo_val, hi_val, is_hard) {
            if let Some(new_expr) = optimize_nested_repetition_simple(child, lo_val, hi_val, greedy_val) {
                *expr = new_expr;
            }
        }
    }
}

/// Simplified version of should_optimize_nested_repetition that doesn't need full Info
fn should_optimize_nested_repetition_simple(
    child: &Expr, 
    _outer_lo: usize, 
    outer_hi: usize,
    is_hard: bool
) -> bool {
    // Only optimize if this is a hard expression (handled by our VM)
    if !is_hard {
        return false;
    }
    
    // Only optimize unbounded outer repetitions (*, +, {n,})
    if outer_hi != usize::MAX {
        return false;
    }
    
    match child {
        // Pattern: (inner_repeat)* or (inner_repeat)+
        Expr::Group(inner) => {
            if matches!(**inner, Expr::Repeat { hi: usize::MAX, .. }) {
                return true;
            }
            // Pattern with overlapping alternatives like (a|ab)*
            if matches!(**inner, Expr::Alt(_)) {
                return has_overlapping_alternatives_simple(inner);
            }
            false
        }
        // Direct nested repetition: repeat* or repeat+
        Expr::Repeat { hi: usize::MAX, .. } => true,
        // Pattern with overlapping alternatives like (a|ab)*
        Expr::Alt(_) => {
            has_overlapping_alternatives_simple(child)
        }
        _ => false,
    }
}

/// Simplified version that doesn't need Info
fn has_overlapping_alternatives_simple(expr: &Expr) -> bool {
    let alternatives = match expr {
        Expr::Alt(alts) => alts,
        Expr::Group(inner) => {
            if let Expr::Alt(alts) = &**inner {
                alts
            } else {
                return false;
            }
        }
        _ => return false,
    };
    
    // Check for overlapping prefixes among alternatives
    for (i, alt1) in alternatives.iter().enumerate() {
        for alt2 in alternatives.iter().skip(i + 1) {
            if is_prefix_of(alt1, alt2) || is_prefix_of(alt2, alt1) {
                return true;
            }
        }
    }
    
    false
}

/// Simplified version of optimize_nested_repetition that doesn't need Info
fn optimize_nested_repetition_simple(
    child: &Expr, 
    outer_lo: usize, 
    outer_hi: usize,
    outer_greedy: bool
) -> Option<Expr> {
    match child {
        // Pattern: (a+)* -> a*  or  (a+)+ -> a+
        Expr::Group(inner) => {
            if matches!(**inner, Expr::Repeat { hi: usize::MAX, .. }) {
                if let Expr::Repeat { child: inner_child, lo: inner_lo, hi: inner_hi, greedy: _inner_greedy } = &**inner {
                    if *inner_hi == usize::MAX && *inner_lo >= 1 {
                        // Transform (a+)* to a* and (a+)+ to a+
                        let new_lo = if outer_lo == 0 { 0 } else { *inner_lo };
                        return Some(Expr::Repeat {
                            child: inner_child.clone(),
                            lo: new_lo,
                            hi: outer_hi,
                            greedy: outer_greedy,
                        });
                    }
                }
            }
            // Pattern: (a|ab)* -> use atomic groups to prevent backtracking
            else if matches!(**inner, Expr::Alt(_)) {
                if has_overlapping_alternatives_simple(inner) {
                    // Wrap the group in an atomic group: (?>a|ab)*
                    let atomic_child = Expr::AtomicGroup(inner.clone());
                    let atomic_group = Box::new(Expr::Group(Box::new(atomic_child)));
                    return Some(Expr::Repeat {
                        child: atomic_group,
                        lo: outer_lo,
                        hi: outer_hi,
                        greedy: outer_greedy,
                    });
                }
            }
        }
        // Pattern: a+* -> a*  or  a++ -> a+
        Expr::Repeat { child: inner_child, lo: inner_lo, hi: inner_hi, greedy: _ } => {
            if *inner_hi == usize::MAX && *inner_lo >= 1 {
                // Transform a+* to a* and a++ to a+
                let new_lo = if outer_lo == 0 { 0 } else { *inner_lo };
                return Some(Expr::Repeat {
                    child: inner_child.clone(),
                    lo: new_lo,
                    hi: outer_hi,
                    greedy: outer_greedy,
                });
            }
        }
        Expr::Alt(_) => {
            if has_overlapping_alternatives_simple(child) {
                // Wrap in atomic group: (?>a|ab)*
                let atomic_child = Expr::AtomicGroup(Box::new(child.clone()));
                return Some(Expr::Repeat {
                    child: Box::new(atomic_child),
                    lo: outer_lo,
                    hi: outer_hi,
                    greedy: outer_greedy,
                });
            }
        }
        _ => {
            // No optimization for other patterns
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::optimize;
    use super::optimize_catastrophic_backtracking;
    use super::optimize_catastrophic_backtracking_expr;
    use super::should_optimize_nested_repetition;
    use super::has_overlapping_alternatives;
    use super::is_prefix_of;
    use super::optimize_nested_repetition;
    use super::vec;
    use super::Box;
    use crate::analyze::analyze;
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

    // Tests for catastrophic backtracking optimization

    #[test]
    fn should_detect_nested_repetition_in_hard_context() {
        // Test detection function works
        let tree = Expr::parse_tree(r"(a+)*\1").unwrap();
        let info = analyze(&tree, 1).unwrap();
        
        // Find the repeat expression in the tree
        if let Expr::Concat(concat) = &tree.expr {
            if let Some(Expr::Repeat { child, lo, hi, .. }) = concat.first() {
                if let Some(child_info) = info.children.first() {
                    let should_opt = should_optimize_nested_repetition(
                        child, 
                        child_info.children.first().unwrap(), 
                        *lo, 
                        *hi, 
                        info.hard
                    );
                    assert!(should_opt, "Should detect nested repetition in hard context");
                }
            }
        }
    }

    #[test]
    fn should_not_detect_nested_repetition_in_easy_context() {
        // Test detection function works for easy context
        let tree = Expr::parse_tree(r"(a+)*b").unwrap();
        let info = analyze(&tree, 1).unwrap();
        
        // Find the repeat expression in the tree
        if let Expr::Concat(concat) = &tree.expr {
            if let Some(Expr::Repeat { child, lo, hi, .. }) = concat.first() {
                if let Some(child_info) = info.children.first() {
                    let should_opt = should_optimize_nested_repetition(
                        child, 
                        child_info.children.first().unwrap(), 
                        *lo, 
                        *hi, 
                        info.hard
                    );
                    assert!(!should_opt, "Should NOT detect nested repetition in easy context");
                }
            }
        }
    }

    #[test]
    fn can_optimize_nested_repetition() {
        // Test the optimization function directly
        let inner_repeat = Expr::Repeat {
            child: Box::new(Expr::Literal { val: "a".to_string(), casei: false }),
            lo: 1,
            hi: usize::MAX,
            greedy: true,
        };
        let group = Expr::Group(Box::new(inner_repeat));
        
        // Create a minimal child_info - we don't use it in the optimization logic
        let tree = Expr::parse_tree(r"a").unwrap();
        let dummy_info = analyze(&tree, 1).unwrap();
        
        let result = optimize_nested_repetition(&group, &dummy_info, 0, usize::MAX, true);
        
        assert!(result.is_some(), "Should optimize nested repetition");
        if let Some(Expr::Repeat { child, lo, hi, greedy }) = result {
            assert_eq!(lo, 0);
            assert_eq!(hi, usize::MAX);
            assert_eq!(greedy, true);
            // Should be the inner literal directly
            assert!(matches!(child.as_ref(), Expr::Literal { .. }));
        }
    }

    #[test] 
    fn can_detect_overlapping_alternatives() {
        let tree = Expr::parse_tree(r"a|ab").unwrap();
        let info = analyze(&tree, 1).unwrap();
        
        let has_overlap = has_overlapping_alternatives(&tree.expr, &info);
        assert!(has_overlap, "Should detect overlapping alternatives a|ab");
    }

    #[test]
    fn prefix_detection_works() {
        let literal_a = Expr::Literal { val: "a".to_string(), casei: false };
        let literal_ab = Expr::Concat(vec![
            Expr::Literal { val: "a".to_string(), casei: false },
            Expr::Literal { val: "b".to_string(), casei: false },
        ]);
        
        assert!(is_prefix_of(&literal_a, &literal_ab), "a should be prefix of ab");
        assert!(!is_prefix_of(&literal_ab, &literal_a), "ab should not be prefix of a");
    }
}
