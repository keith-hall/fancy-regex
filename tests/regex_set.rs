use fancy_regex::{RegexSet, RegexSetBuilder};

#[test]
fn basic_regex_set() {
    let set = RegexSet::new(&[r"\d+", r"\w+"]).unwrap();
    assert!(set.is_match("hello").unwrap());
    assert!(set.is_match("123").unwrap());
    assert!(!set.is_match("!!!").unwrap());
}

#[test]
fn regex_set_matches() {
    let set = RegexSet::new(&[r"\d+", r"\w+"]).unwrap();
    
    // "\w+" matches "hello", and it's pattern index 1
    let matches = set.matches("hello").unwrap();
    assert_eq!(matches.matched_pattern(), Some(1));
    assert!(matches.matched_any());

    // "\d+" matches "123", and it's pattern index 0
    let matches = set.matches("123").unwrap();
    assert_eq!(matches.matched_pattern(), Some(0));
    assert!(matches.matched_any());

    // No match
    let matches = set.matches("!!!").unwrap();
    assert_eq!(matches.matched_pattern(), None);
    assert!(!matches.matched_any());
}

#[test]
fn regex_set_priority_order() {
    // When both patterns match, the first one should win
    let set = RegexSet::new(&[r"foo", r"\w+"]).unwrap();
    
    let matches = set.matches("foo").unwrap();
    // "foo" is more specific and comes first, so it should match as pattern 0
    assert_eq!(matches.matched_pattern(), Some(0));

    let matches = set.matches("bar").unwrap();
    // "bar" only matches "\w+", which is pattern 1
    assert_eq!(matches.matched_pattern(), Some(1));
}

#[test]
fn regex_set_with_backreferences() {
    // Test with a "hard" regex that requires backtracking
    let set = RegexSet::new(&[r"(\w+) \1", r"\d+"]).unwrap();
    
    // "foo foo" matches the first pattern
    let matches = set.matches("foo foo").unwrap();
    assert_eq!(matches.matched_pattern(), Some(0));

    // "123" matches the second pattern
    let matches = set.matches("123").unwrap();
    assert_eq!(matches.matched_pattern(), Some(1));
}

#[test]
fn regex_set_with_lookahead() {
    let set = RegexSet::new(&[r"\w+(?=!)", r"\d+"]).unwrap();
    
    // "hello!" matches the first pattern (word followed by !)
    let matches = set.matches("hello!").unwrap();
    assert_eq!(matches.matched_pattern(), Some(0));

    // "123" matches the second pattern
    let matches = set.matches("123").unwrap();
    assert_eq!(matches.matched_pattern(), Some(1));
}

#[test]
fn regex_set_builder() {
    let set = RegexSetBuilder::new(&[r"FOO", r"BAR"])
        .case_insensitive(true)
        .build()
        .unwrap();
    
    assert!(set.is_match("foo").unwrap());
    assert!(set.is_match("bar").unwrap());
    assert!(!set.is_match("baz").unwrap());

    let matches = set.matches("foo").unwrap();
    assert_eq!(matches.matched_pattern(), Some(0));
}

#[test]
fn regex_set_len() {
    let set = RegexSet::new(&[r"\d+", r"\w+", r"foo"]).unwrap();
    assert_eq!(set.len(), 3);
    assert!(!set.is_empty());
}

#[test]
fn regex_set_empty_error() {
    let empty: Vec<&str> = vec![];
    let result = RegexSet::new(&empty);
    assert!(result.is_err());
}

#[test]
fn regex_set_multiple_patterns() {
    let set = RegexSet::new(&[
        r"^error:",
        r"^warning:",
        r"^info:",
        r"\d{4}-\d{2}-\d{2}",
    ]).unwrap();
    
    let matches = set.matches("error: something went wrong").unwrap();
    assert_eq!(matches.matched_pattern(), Some(0));
    
    let matches = set.matches("warning: be careful").unwrap();
    assert_eq!(matches.matched_pattern(), Some(1));
    
    let matches = set.matches("Date: 2024-01-15").unwrap();
    assert_eq!(matches.matched_pattern(), Some(3));
}
