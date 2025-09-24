//! Integration tests for catastrophic backtracking optimization

use fancy_regex::Regex;

#[test]
fn test_nested_repetition_with_backref() {
    // Pattern that should be optimized: (a+)* with subsequent backref to the inner group
    let regex = Regex::new(r"(a+)(a+)*\1").unwrap();
    
    // Should still work correctly after optimization
    let result = regex.captures("aaaa").unwrap();
    assert!(result.is_some());
}

#[test]
fn test_direct_nested_repetition() {
    // Test direct nested repetition a+* -> a*
    // This is hard to test directly because a+* is not valid syntax in most regex flavors
    // But our optimization should handle nested structures in hard contexts
    let regex = Regex::new(r"((\w+)*)*\1").unwrap();
    
    // Should work without hanging or error
    assert!(!regex.is_match("hello").unwrap());
}

#[test]
fn test_overlapping_alternatives() {
    // Pattern with overlapping alternatives that could cause catastrophic backtracking
    let regex = Regex::new(r"(a|ab)*b(\1)").unwrap();
    
    // Should still work correctly after optimization
    assert!(regex.is_match("ab").unwrap());
}

#[test] 
fn test_easy_pattern_not_affected() {
    // Pattern without backrefs should not be affected by the optimization
    let regex = Regex::new(r"(a+)*b").unwrap();
    
    // Should work as expected
    assert!(regex.is_match("aaaaab").unwrap());
    assert!(regex.is_match("b").unwrap());
    assert!(!regex.is_match("c").unwrap());
}

#[test]
fn test_complex_nested_pattern() {
    // More complex nested pattern with backref - this was in the original problem statement
    let regex = Regex::new(r"(\w+\s?)*(\1)").unwrap();
    
    // Should not crash or take excessive time
    assert!(!regex.is_match("hello world hello").unwrap());
}

#[test] 
fn test_performance_regression() {
    // Based on the original problem example
    let regex = Regex::new(r"(a|b|ab)*bc").unwrap();
    
    // This should complete quickly instead of taking exponential time
    let test_str = "ab".repeat(20) + "ac";
    let result = regex.is_match(&test_str);
    assert!(result.is_ok());
    assert!(!result.unwrap()); // Should not match since it ends with "ac", not "bc"
}