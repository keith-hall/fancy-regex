use fancy_regex::MatchContext;

mod common;

#[test]
fn test_basic_match_context() {
    let regex = common::regex(r"\d+");
    let mut ctx = MatchContext::new();
    
    // Test the new method
    let result = regex.captures_from_pos_with_context("abc123def", 0, &mut ctx);
    assert!(result.is_ok());
    
    let captures = result.unwrap();
    assert!(captures.is_some());
    
    let captures = captures.unwrap();
    assert_eq!(captures.get(0).unwrap().as_str(), "123");
}

#[test]
fn test_compare_original_vs_context() {
    let regex = common::regex(r"\d+");
    let text = "abc123def";
    
    // Test original method
    let original = regex.captures_from_pos(text, 0).unwrap();
    
    // Test new method
    let mut ctx = MatchContext::new();
    let with_context = regex.captures_from_pos_with_context(text, 0, &mut ctx).unwrap();
    
    // Both should produce the same result
    let both_found = original.is_some() && with_context.is_some();
    let both_not_found = original.is_none() && with_context.is_none();
    
    assert!(both_found || both_not_found, 
        "Results differ: original found={}, context found={}", 
        original.is_some(), with_context.is_some());
    
    if let (Some(o), Some(c)) = (original, with_context) {
        assert_eq!(o.get(0).unwrap().as_str(), c.get(0).unwrap().as_str());
    }
}

#[test]
fn test_vm_optimization_with_backreference() {
    // This will use the fancy VM since it has a backreference
    let regex = common::regex(r"(\d)\1");
    let mut ctx = MatchContext::new();
    
    // Test the new method
    let captures = regex.captures_from_pos_with_context("11 22 33", 0, &mut ctx)
        .unwrap()
        .unwrap();
    assert_eq!(captures.get(0).unwrap().as_str(), "11");
    assert_eq!(captures.get(1).unwrap().as_str(), "1");
    
    // Reuse context for another match
    let captures2 = regex.captures_from_pos_with_context("44 55 66", 0, &mut ctx)
        .unwrap()
        .unwrap();
    assert_eq!(captures2.get(0).unwrap().as_str(), "44");
    assert_eq!(captures2.get(1).unwrap().as_str(), "4");
}

#[test]
fn test_vm_optimization_with_lookbehind() {
    // This will use the fancy VM since it has a lookbehind
    let regex = common::regex(r"(?<=\w)\d+");
    let mut ctx = MatchContext::new();
    
    // Test the new method
    let captures = regex.captures_from_pos_with_context("a123 b456", 0, &mut ctx)
        .unwrap()
        .unwrap();
    assert_eq!(captures.get(0).unwrap().as_str(), "123");
    
    // Reuse context for another match
    let captures2 = regex.captures_from_pos_with_context("c789 d000", 0, &mut ctx)
        .unwrap()
        .unwrap();
    assert_eq!(captures2.get(0).unwrap().as_str(), "789");
}

#[test]
fn test_context_reuse_equivalence() {
    // Test with a pattern that uses fancy features
    let regex = common::regex(r"(\w+)\s+\1");
    let text1 = "hello hello world";
    let text2 = "test test more";
    
    // Test original method
    let original1 = regex.captures_from_pos(text1, 0).unwrap();
    let original2 = regex.captures_from_pos(text2, 0).unwrap();
    
    // Test new method with context reuse
    let mut ctx = MatchContext::new();
    let context1 = regex.captures_from_pos_with_context(text1, 0, &mut ctx).unwrap();
    let context2 = regex.captures_from_pos_with_context(text2, 0, &mut ctx).unwrap();
    
    // Results should be identical
    match (original1, context1) {
        (Some(o), Some(c)) => {
            assert_eq!(o.get(0).unwrap().as_str(), c.get(0).unwrap().as_str());
            assert_eq!(o.get(1).unwrap().as_str(), c.get(1).unwrap().as_str());
        }
        (None, None) => {}
        _ => panic!("Results differ for text1"),
    }
    
    match (original2, context2) {
        (Some(o), Some(c)) => {
            assert_eq!(o.get(0).unwrap().as_str(), c.get(0).unwrap().as_str());
            assert_eq!(o.get(1).unwrap().as_str(), c.get(1).unwrap().as_str());
        }
        (None, None) => {}
        _ => panic!("Results differ for text2"),
    }
}