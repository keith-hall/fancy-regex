// Tests for patterns with {0} repetitions

use fancy_regex::Regex;

#[test]
fn test_zero_repetition_with_lookbehind() {
    let re = Regex::new(r"(?<=)(abc)(ABC){0}").unwrap();
    assert!(re.is_match("abc").unwrap());
    
    let caps = re.captures("abc").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "abc");
    assert_eq!(caps.get(1).unwrap().as_str(), "abc");
    assert!(caps.get(2).is_none(), "Group 2 should be None as it's inside {{0}}");
}

#[test]
fn test_zero_repetition_simple() {
    let re = Regex::new(r"(abc)(ABC){0}").unwrap();
    assert!(re.is_match("abc").unwrap());
    
    let caps = re.captures("abc").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "abc");
    assert_eq!(caps.get(1).unwrap().as_str(), "abc");
    assert!(caps.get(2).is_none(), "Group 2 should be None as it's inside {{0}}");
}

#[test]
fn test_zero_repetition_multiple_groups() {
    let re = Regex::new(r"(a)(b)(c){0}(d){0}").unwrap();
    assert!(re.is_match("ab").unwrap());
    
    let caps = re.captures("ab").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "ab");
    assert_eq!(caps.get(1).unwrap().as_str(), "a");
    assert_eq!(caps.get(2).unwrap().as_str(), "b");
    assert!(caps.get(3).is_none());
    assert!(caps.get(4).is_none());
}

#[test]
fn test_zero_repetition_no_match() {
    let re = Regex::new(r"(abc)(ABC){0}").unwrap();
    assert!(!re.is_match("ABC").unwrap());
    assert!(!re.is_match("").unwrap());
}

#[test]
fn test_zero_repetition_nested() {
    let re = Regex::new(r"(a)((b)(c)){0}").unwrap();
    assert!(re.is_match("a").unwrap());
    
    let caps = re.captures("a").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "a");
    assert_eq!(caps.get(1).unwrap().as_str(), "a");
    assert!(caps.get(2).is_none());
    assert!(caps.get(3).is_none());
    assert!(caps.get(4).is_none());
}

#[test]
fn test_zero_repetition_with_alternation() {
    let re = Regex::new(r"(x|y)(a|b){0}").unwrap();
    assert!(re.is_match("x").unwrap());
    assert!(re.is_match("y").unwrap());
    assert!(!re.is_match("a").unwrap());
    
    let caps = re.captures("x").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "x");
    assert_eq!(caps.get(1).unwrap().as_str(), "x");
    assert!(caps.get(2).is_none());
}

#[test]
fn test_zero_repetition_after_lookahead() {
    let re = Regex::new(r"(abc)(?=def)(xyz){0}").unwrap();
    assert!(re.is_match("abcdef").unwrap());
    
    let caps = re.captures("abcdef").unwrap().unwrap();
    assert_eq!(caps.get(0).unwrap().as_str(), "abc");
    assert_eq!(caps.get(1).unwrap().as_str(), "abc");
    assert!(caps.get(2).is_none());
}
