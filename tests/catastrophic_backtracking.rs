use fancy_regex::{Regex, Result};
use std::time::{Duration, Instant};

#[test]
fn test_nested_quantifiers_prevention() -> Result<()> {
    // This pattern could cause catastrophic backtracking: (a+)+
    let re = Regex::new(r"(a+)+")?;
    
    // This input would cause exponential behavior without optimization
    let pathological_input = "aaaaaaaaaaaaaaaaaaaaaaaaaaab"; // 27 a's + b
    
    let start = Instant::now();
    let result = re.is_match(pathological_input)?;
    let duration = start.elapsed();
    
    // Should match the 'a's part
    assert!(result);
    
    // Should complete in reasonable time (less than 10ms for this input size)
    assert!(duration < Duration::from_millis(10), 
           "Nested quantifier pattern took {:?}, expected < 10ms", duration);
    
    Ok(())
}

#[test]
fn test_overlapping_alternation_prevention() -> Result<()> {
    // This pattern could cause catastrophic backtracking: (a|b|ab)*bc
    let re = Regex::new(r"(a|b|ab)*bc")?;
    
    // This input would cause exponential behavior without optimization
    let pathological_input = &("ab".repeat(15) + "ac"); // abab...ac (no match)
    
    let start = Instant::now();
    let result = re.is_match(pathological_input)?;
    let duration = start.elapsed();
    
    // Should not match (ends with 'ac' not 'bc')
    assert!(!result);
    
    // Should complete in reasonable time
    assert!(duration < Duration::from_millis(10),
           "Overlapping alternation pattern took {:?}, expected < 10ms", duration);
    
    Ok(())
}

#[test]
fn test_duplicate_alternation_removal() -> Result<()> {
    // This pattern has redundant alternatives: (a|a)*
    let re = Regex::new(r"^(a|a)*$")?;
    
    let test_cases = [
        ("", true),      // empty string should match
        ("a", true),     // single 'a' should match
        ("aaa", true),   // multiple 'a's should match
        ("ab", false),   // 'b' should not match
    ];
    
    for (input, expected) in test_cases {
        let start = Instant::now();
        let result = re.is_match(input)?;
        let duration = start.elapsed();
        
        assert_eq!(result, expected, "Failed for input: '{}'", input);
        assert!(duration < Duration::from_millis(5),
               "Duplicate alternation took {:?} for '{}', expected < 5ms", duration, input);
    }
    
    Ok(())
}

#[test]
fn test_sequential_quantifiers_optimization() -> Result<()> {
    // This pattern has redundant sequential quantifiers: \w*\w*
    let re = Regex::new(r"\w*\w*")?;
    
    let test_cases = [
        ("", true),           // empty should match
        ("abc", true),        // word chars should match  
        ("123", true),        // digits should match
        ("abc123", true),     // mixed should match
        ("!", true),          // should match empty prefix before non-word char
    ];
    
    for (input, expected) in test_cases {
        let start = Instant::now();
        let result = re.is_match(input)?;
        let duration = start.elapsed();
        
        assert_eq!(result, expected, "Failed for input: '{}'", input);
        assert!(duration < Duration::from_millis(1),
               "Sequential quantifiers took {:?} for '{}', expected < 1ms", duration, input);
    }
    
    Ok(())
}

#[test] 
fn test_complex_nested_pattern_optimization() -> Result<()> {
    // Complex pattern that combines multiple issues: (\w+\s?)*
    let re = Regex::new(r"(\w+\s?)*")?;
    
    let test_input = "word1 word2 word3 ";
    
    let start = Instant::now();
    let result = re.is_match(test_input)?;
    let duration = start.elapsed();
    
    // Should match
    assert!(result);
    
    // Should complete quickly
    assert!(duration < Duration::from_millis(5),
           "Complex nested pattern took {:?}, expected < 5ms", duration);
    
    Ok(())
}

#[test]
fn test_correctness_preservation() -> Result<()> {
    // Test that optimizations preserve the original regex semantics
    let test_cases = [
        // Pattern, input, should_match
        (r"(a+)+", "aaa", true),
        (r"(a+)+", "bbb", false),
        (r"(a|b|ab)*bc", "ababbc", true),
        (r"(a|b|ab)*bc", "ababac", false),
        (r"^(a|a)*$", "aaa", true),
        (r"^(a|a)*$", "aab", false),
        (r"\w*\w*", "abc123", true),
        (r"\w*\w*", "", true),
        (r"(\w+\s?)*", "hello world", true),
        (r"(\w+\s?)*", "", true),
    ];
    
    for (pattern, input, expected) in test_cases {
        let re = Regex::new(pattern)?;
        let result = re.is_match(input)?;
        assert_eq!(result, expected, 
                  "Correctness failed for pattern '{}' with input '{}': expected {}, got {}", 
                  pattern, input, expected, result);
    }
    
    Ok(())
}

#[test]
fn test_performance_improvement_demonstration() -> Result<()> {
    // Demonstrate performance improvement on patterns that would be problematic
    // without optimization
    
    struct TestCase {
        description: &'static str,
        pattern: &'static str,
        input: String,
        max_duration_ms: u64,
    }
    
    let test_cases = vec![
        TestCase {
            description: "Nested quantifiers with long input",
            pattern: r"(a+)+",
            input: "a".repeat(30) + "b",
            max_duration_ms: 20,
        },
        TestCase {
            description: "Overlapping alternation with pathological input",
            pattern: r"(a|ab)*bc",
            input: "ab".repeat(20) + "ac",
            max_duration_ms: 20,
        },
        TestCase {
            description: "Deep nesting with multiple quantifiers",
            pattern: r"((a+)+)+",
            input: "a".repeat(25) + "b",
            max_duration_ms: 30,
        },
    ];
    
    for test_case in test_cases {
        println!("Testing: {}", test_case.description);
        println!("Pattern: {}", test_case.pattern);
        
        let re = Regex::new(test_case.pattern)?;
        let start = Instant::now();
        let _result = re.is_match(&test_case.input)?;
        let duration = start.elapsed();
        
        println!("Duration: {:?}", duration);
        assert!(duration < Duration::from_millis(test_case.max_duration_ms),
               "{} took {:?}, expected < {}ms", 
               test_case.description, duration, test_case.max_duration_ms);
        println!("✓ Performance test passed\n");
    }
    
    Ok(())
}

#[test]
fn test_optimization_does_not_break_valid_patterns() -> Result<()> {
    // Ensure that patterns that should not be optimized still work correctly
    let valid_patterns = [
        (r"abc", "abc", true),
        (r"a+", "aaa", true),
        (r"a|b", "b", true),
        (r"a*b*", "aaabbb", true),
        (r"(abc)+", "abcabc", true),
        (r"[a-z]+", "hello", true),
        (r"\d{3}-\d{3}-\d{4}", "123-456-7890", true),
        (r"(?i:hello)", "HELLO", true),
    ];
    
    for (pattern, input, expected) in valid_patterns {
        let re = Regex::new(pattern)?;
        let result = re.is_match(input)?;
        assert_eq!(result, expected,
                  "Valid pattern '{}' failed on input '{}': expected {}, got {}",
                  pattern, input, expected, result);
    }
    
    Ok(())
}