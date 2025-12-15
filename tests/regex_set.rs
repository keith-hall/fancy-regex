use fancy_regex::RegexSet;

#[test]
fn test_basic_matching() {
    let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();
    
    // Test digit pattern
    let result = set.matches("123").unwrap();
    assert!(result.matched(0));
    assert!(!result.matched(1));
    
    // Test letter pattern
    let result = set.matches("abc").unwrap();
    assert!(!result.matched(0));
    assert!(result.matched(1));
    
    // Test no match
    let result = set.matches("!!!").unwrap();
    assert!(!result.matched(0));
    assert!(!result.matched(1));
    assert_eq!(result.pattern(), None);
}

#[test]
fn test_priority_order() {
    // Both patterns could match digits, but pattern 0 has priority
    let set = RegexSet::new(&[r"\d+", r"[0-9]+"]).unwrap();
    let result = set.matches("123").unwrap();
    assert!(result.matched(0));
    assert!(!result.matched(1));
    
    // More specific pattern comes first
    let set = RegexSet::new(&[r"\d{4}", r"\d+"]).unwrap();
    let result = set.matches("2024").unwrap();
    assert!(result.matched(0));
    assert_eq!(result.pattern(), Some(0));
}

#[test]
fn test_is_match() {
    let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();
    assert!(set.is_match("123").unwrap());
    assert!(set.is_match("abc").unwrap());
    assert!(!set.is_match("!!!").unwrap());
}

#[test]
fn test_find() {
    let set = RegexSet::new(&[r"\d{4}", r"\d+"]).unwrap();
    let (pattern_idx, mat) = set.find("The year 2024 has 365 days").unwrap().unwrap();
    
    assert_eq!(pattern_idx, 0);
    assert_eq!(mat.as_str(), "2024");
    assert_eq!(mat.start(), 9);
    assert_eq!(mat.end(), 13);
}

#[test]
fn test_find_with_second_pattern() {
    let set = RegexSet::new(&[r"\d{4}", r"\w+"]).unwrap();
    let (pattern_idx, mat) = set.find("hello world").unwrap().unwrap();
    
    assert_eq!(pattern_idx, 1); // Only second pattern matches
    assert_eq!(mat.as_str(), "hello");
}

#[test]
fn test_captures_with_groups() {
    let set = RegexSet::new(&[
        r"(\d{4})-(\d{2})-(\d{2})", // Date with capture groups
        r"\w+",
    ]).unwrap();
    
    // Use text where the date pattern matches first
    let (pattern_idx, captures) = set.captures("2024-12-15 is the date").unwrap().unwrap();
    assert_eq!(pattern_idx, 0);
    
    // Group 0 is the entire match
    assert_eq!(captures.get(0).unwrap().as_str(), "2024-12-15");
    
    // Group 1 is the wrapper for pattern 0 (contains the full date match)
    assert_eq!(captures.get(1).unwrap().as_str(), "2024-12-15");
    
    // Groups 2-4 are the capture groups within the date pattern
    assert_eq!(captures.get(2).unwrap().as_str(), "2024");
    assert_eq!(captures.get(3).unwrap().as_str(), "12");
    assert_eq!(captures.get(4).unwrap().as_str(), "15");
}

#[test]
fn test_empty_set() {
    let set = RegexSet::new(&[]).unwrap();
    assert_eq!(set.len(), 0);
    assert!(set.is_empty());
    assert!(!set.is_match("anything").unwrap());
}

#[test]
fn test_single_pattern() {
    let set = RegexSet::new(&[r"\d+"]).unwrap();
    assert_eq!(set.len(), 1);
    assert!(!set.is_empty());
    
    let result = set.matches("123").unwrap();
    assert!(result.matched(0));
}

#[test]
fn test_complex_patterns() {
    let set = RegexSet::new(&[
        r"\d{4}-\d{2}-\d{2}",  // Date
        r"\w+@\w+\.\w+",        // Email
        r"\d{3}-\d{3}-\d{4}",   // Phone (simplified)
    ]).unwrap();
    
    // Test date
    let result = set.matches("2024-12-15").unwrap();
    assert_eq!(result.pattern(), Some(0));
    
    // Test email
    let result = set.matches("user@example.com").unwrap();
    assert_eq!(result.pattern(), Some(1));
    
    // Test phone
    let result = set.matches("123-456-7890").unwrap();
    assert_eq!(result.pattern(), Some(2));
}

#[test]
fn test_fancy_features() {
    // Test with lookahead
    let set = RegexSet::new(&[
        r"\w+(?=\d)",  // Word followed by digit
        r"\w+",        // Any word
    ]).unwrap();
    
    let result = set.matches("test123").unwrap();
    assert_eq!(result.pattern(), Some(0)); // First pattern has priority and matches
}

#[test]
fn test_backreferences() {
    // Test with backreferences
    let set = RegexSet::new(&[
        r"(\w+)\s+\1",  // Repeated word
        r"\w+",         // Any word
    ]).unwrap();
    
    let result = set.matches("hello hello").unwrap();
    assert_eq!(result.pattern(), Some(0));
    
    let result = set.matches("hello world").unwrap();
    assert_eq!(result.pattern(), Some(1));
}

#[test]
fn test_match_location() {
    let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();
    let result = set.matches("abc").unwrap();
    
    let (start, end) = result.match_range().unwrap();
    assert_eq!(start, 0);
    assert_eq!(end, 3);
}

#[test]
fn test_no_match_location() {
    let set = RegexSet::new(&[r"\d+"]).unwrap();
    let result = set.matches("abc").unwrap();
    
    assert_eq!(result.match_range(), None);
}

#[test]
fn test_alternation_in_patterns() {
    let set = RegexSet::new(&[
        r"(cat|dog)",
        r"(bird|fish)",
    ]).unwrap();
    
    let result = set.matches("cat").unwrap();
    assert_eq!(result.pattern(), Some(0));
    
    let result = set.matches("bird").unwrap();
    assert_eq!(result.pattern(), Some(1));
}
