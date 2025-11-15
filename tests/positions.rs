// Tests for position information tracking in the parser

use fancy_regex::Expr;

#[test]
fn test_simple_positions() {
    // Test: `^\s\b.` should have:
    // - anchor at position 0
    // - whitespace (\s) at position 1
    // - word boundary (\b) at position 3
    // - dot at position 5
    let tree = Expr::parse_tree(r"^\s\b.").unwrap();
    
    // The root is a Concat
    if let Expr::Concat(children) = &tree.expr.expr {
        assert_eq!(children.len(), 4, "Should have 4 children");
        
        // First child: ^ anchor at position 0
        assert_eq!(children[0].ix, 0, "Anchor should be at position 0");
        match &children[0].expr {
            Expr::Assertion(_) => (),
            _ => panic!("Expected Assertion"),
        }
        
        // Second child: \s at position 1
        assert_eq!(children[1].ix, 1, "\\s should be at position 1");
        match &children[1].expr {
            Expr::Delegate { .. } => (),
            _ => panic!("Expected Delegate for \\s"),
        }
        
        // Third child: \b at position 3
        assert_eq!(children[2].ix, 3, "\\b should be at position 3");
        match &children[2].expr {
            Expr::Assertion(_) => (),
            _ => panic!("Expected Assertion for \\b"),
        }
        
        // Fourth child: . at position 5
        assert_eq!(children[3].ix, 5, ". should be at position 5");
        match &children[3].expr {
            Expr::Any { .. } => (),
            _ => panic!("Expected Any for ."),
        }
    } else {
        panic!("Expected Concat at root");
    }
}

#[test]
fn test_literal_positions() {
    // Test: `abc` - each literal should have its position
    let tree = Expr::parse_tree("abc").unwrap();
    
    if let Expr::Concat(children) = &tree.expr.expr {
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].ix, 0, "First 'a' at position 0");
        assert_eq!(children[1].ix, 1, "Second 'b' at position 1");
        assert_eq!(children[2].ix, 2, "Third 'c' at position 2");
    } else {
        panic!("Expected Concat");
    }
}

#[test]
fn test_group_positions() {
    // Test: `(ab)` - group should start at position 0, content at 1
    let tree = Expr::parse_tree("(ab)").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Group should start at position 0");
    
    if let Expr::Group(inner) = &tree.expr.expr {
        // The inner Concat should start at position 1
        assert_eq!(inner.ix, 1, "Inner content should start at position 1");
        
        if let Expr::Concat(children) = &inner.expr {
            assert_eq!(children[0].ix, 1, "'a' at position 1");
            assert_eq!(children[1].ix, 2, "'b' at position 2");
        } else {
            panic!("Expected Concat inside group");
        }
    } else {
        panic!("Expected Group");
    }
}

#[test]
fn test_alternation_positions() {
    // Test: `a|b` - alt should start at 0, each branch at its position
    let tree = Expr::parse_tree("a|b").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Alt should start at position 0");
    
    if let Expr::Alt(branches) = &tree.expr.expr {
        assert_eq!(branches.len(), 2);
        assert_eq!(branches[0].ix, 0, "First branch 'a' at position 0");
        assert_eq!(branches[1].ix, 2, "Second branch 'b' at position 2");
    } else {
        panic!("Expected Alt");
    }
}

#[test]
fn test_repeat_positions() {
    // Test: `a*` - repeat should start at 0, child 'a' at 0
    let tree = Expr::parse_tree("a*").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Repeat should start at position 0");
    
    if let Expr::Repeat { child, .. } = &tree.expr.expr {
        assert_eq!(child.ix, 0, "Child 'a' should be at position 0");
    } else {
        panic!("Expected Repeat");
    }
}

#[test]
fn test_lookahead_positions() {
    // Test: `(?=a)` - lookahead group starts at 0, content at 3
    let tree = Expr::parse_tree("(?=a)").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Lookahead should start at position 0");
    
    if let Expr::LookAround(inner, _) = &tree.expr.expr {
        assert_eq!(inner.ix, 3, "Content 'a' should be at position 3");
    } else {
        panic!("Expected LookAround");
    }
}

#[test]
fn test_character_class_positions() {
    // Test: `[abc]` - delegate should be at position 0
    let tree = Expr::parse_tree("[abc]").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Character class should start at position 0");
    
    match &tree.expr.expr {
        Expr::Delegate { .. } => (),
        _ => panic!("Expected Delegate for character class"),
    }
}

#[test]
fn test_backref_positions() {
    // Test: `(a)\1` - backref \1 should be at position 3 (where \ starts)
    let tree = Expr::parse_tree(r"(a)\1").unwrap();
    
    if let Expr::Concat(children) = &tree.expr.expr {
        assert_eq!(children.len(), 2);
        
        // Group at position 0
        assert_eq!(children[0].ix, 0, "Group at position 0");
        
        // Backref \1 starts at position 3 (where \ is)
        assert_eq!(children[1].ix, 3, "Backref should be at position 3 (start of \\1)");
        match &children[1].expr {
            Expr::Backref { .. } => (),
            _ => panic!("Expected Backref"),
        }
    } else {
        panic!("Expected Concat");
    }
}

#[test]
fn test_anchor_positions() {
    // Test: `^a$` - anchors at 0 and 2
    let tree = Expr::parse_tree("^a$").unwrap();
    
    if let Expr::Concat(children) = &tree.expr.expr {
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].ix, 0, "Start anchor at position 0");
        assert_eq!(children[1].ix, 1, "Literal 'a' at position 1");
        assert_eq!(children[2].ix, 2, "End anchor at position 2");
    } else {
        panic!("Expected Concat");
    }
}

#[test]
fn test_atomic_group_positions() {
    // Test: `(?>a)` - atomic group starts at 0, content at 3
    let tree = Expr::parse_tree("(?>a)").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Atomic group should start at position 0");
    
    if let Expr::AtomicGroup(inner) = &tree.expr.expr {
        assert_eq!(inner.ix, 3, "Content 'a' should be at position 3");
    } else {
        panic!("Expected AtomicGroup");
    }
}

#[test]
fn test_escape_sequences_positions() {
    // Test: `\x41` - should track position of the escape
    let tree = Expr::parse_tree(r"\x41").unwrap();
    
    assert_eq!(tree.expr.ix, 0, "Escape sequence should start at position 0");
    
    match &tree.expr.expr {
        Expr::Literal { val, .. } => {
            assert_eq!(val, "A", "\\x41 should be literal 'A'");
        }
        _ => panic!("Expected Literal"),
    }
}
