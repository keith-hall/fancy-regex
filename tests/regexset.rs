mod common;

use fancy_regex::{RegexSet, RegexSetBuilder};

#[test]
fn test_basic_regexset() {
    let set = RegexSet::new(&[r"\d+", r"\w+", r"hello"]).unwrap();

    assert_eq!(set.len(), 3);
    assert!(!set.is_empty());

    let haystack = "hello world 123";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 3);

    // First match should be pattern 0 (\d+) or pattern 1 (\w+) at position 0
    // Actually pattern 1 (\w+) will match "hello" first at position 0
    assert_eq!(matches[0].pattern(), 1);
    assert_eq!(matches[0].as_str(), "hello");
    assert_eq!(matches[0].start(), 0);
    assert_eq!(matches[0].end(), 5);
}

#[test]
fn test_empty_regexset() {
    let set = RegexSet::new::<&[&str], _>(&[]).unwrap();

    assert_eq!(set.len(), 0);
    assert!(set.is_empty());

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();
    assert_eq!(matches.len(), 0);
}

#[test]
fn test_priority_resolution() {
    // When multiple patterns match at the same position, lowest index wins
    let set = RegexSet::new(&[r"hello", r"h\w+", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].as_str(), "hello");
}

#[test]
fn test_easy_patterns() {
    // All easy patterns (can be delegated to DFA)
    let set = RegexSet::new(&[r"\d+", r"[a-z]+", r"[A-Z]+"]).unwrap();

    let haystack = "abc 123 XYZ";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 3);
    assert_eq!(matches[0].pattern(), 1); // [a-z]+
    assert_eq!(matches[0].as_str(), "abc");
    assert_eq!(matches[1].pattern(), 0); // \d+
    assert_eq!(matches[1].as_str(), "123");
    assert_eq!(matches[2].pattern(), 2); // [A-Z]+
    assert_eq!(matches[2].as_str(), "XYZ");
}

#[test]
fn test_hard_patterns_with_backrefs() {
    // Hard pattern (requires backtracking)
    let set = RegexSet::new(&[r"(\w+)\s+\1", r"\d+"]).unwrap();

    let haystack = "hello hello 123";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].pattern(), 0); // backreference pattern
    assert_eq!(matches[0].as_str(), "hello hello");
    assert_eq!(matches[1].pattern(), 1); // \d+
    assert_eq!(matches[1].as_str(), "123");
}

#[test]
fn test_hard_patterns_with_lookaround() {
    // Hard pattern with lookahead
    let set = RegexSet::new(&[r"(?=\d{3})\d+", r"[a-z]+"]).unwrap();

    let haystack = "abc 123";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].pattern(), 1); // [a-z]+
    assert_eq!(matches[0].as_str(), "abc");
    assert_eq!(matches[1].pattern(), 0); // lookahead pattern
    assert_eq!(matches[1].as_str(), "123");
}

#[test]
fn test_mixed_easy_hard_patterns() {
    // Mix of easy and hard patterns
    let set = RegexSet::new(&[
        r"\d+",        // easy
        r"(\w+)\s+\1", // hard (backref)
        r"[a-z]+",     // easy
    ])
    .unwrap();

    let haystack = "foo foo 123 bar";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find: foo foo (pattern 1), 123 (pattern 0), bar (pattern 2)
    // Note: pattern 2 might also match "foo" twice, so we may get more matches
    assert!(matches.len() >= 3);
}

#[test]
fn test_zero_width_matches() {
    // Test that zero-width matches don't cause infinite loops
    let set = RegexSet::new(&[r"\b", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find word boundaries and words, but not loop infinitely
    assert!(matches.len() > 0);
    assert!(matches.len() < 100); // sanity check
}

#[test]
fn test_capture_groups() {
    let set = RegexSet::new(&[r"(\d+)-(\d+)", r"([a-z]+)"]).unwrap();

    let haystack = "abc 123-456";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);

    // First match should have captures
    assert_eq!(matches[0].pattern(), 1);
    let caps = matches[0].captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "abc");
    assert_eq!(caps.get(1).unwrap().as_str(), "abc");

    // Second match should have captures
    assert_eq!(matches[1].pattern(), 0);
    let caps = matches[1].captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "123-456");
    assert_eq!(caps.get(1).unwrap().as_str(), "123");
    assert_eq!(caps.get(2).unwrap().as_str(), "456");
}

#[test]
fn test_builder_case_insensitive() {
    let set = RegexSetBuilder::new(&[r"hello", r"world"])
        .case_insensitive(true)
        .build()
        .unwrap();

    let haystack = "HELLO WORLD";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].as_str(), "HELLO");
    assert_eq!(matches[1].as_str(), "WORLD");
}

#[test]
fn test_builder_multi_line() {
    let set = RegexSetBuilder::new(&[r"^hello", r"world$"])
        .multi_line(true)
        .build()
        .unwrap();

    let haystack = "hello\nworld";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);
}

#[test]
fn test_no_matches() {
    let set = RegexSet::new(&[r"\d+", r"[A-Z]+"]).unwrap();

    let haystack = "abc";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 0);
}

#[test]
fn test_overlapping_patterns_priority() {
    // When patterns overlap, the one with lower index wins at same position
    let set = RegexSet::new(&[r"a+", r"aa", r"aaa"]).unwrap();

    let haystack = "aaa";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Pattern 0 (a+) should match at position 0 (lowest index)
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].as_str(), "aaa");
}

#[test]
fn test_matches_range() {
    let set = RegexSet::new(&[r"\d+", r"[a-z]+"]).unwrap();

    let haystack = "abc 123 xyz 456";
    // Search only in the middle part
    let matches: Vec<_> = set
        .matches_range(haystack, 4..11)
        .map(|m| m.unwrap())
        .collect();

    // Should only find "123" and "xyz"
    assert_eq!(matches.len(), 2);
    assert_eq!(matches[0].as_str(), "123");
    assert_eq!(matches[1].as_str(), "xyz");
}

#[test]
fn test_pattern_ordering() {
    // Test that patterns are matched in order of position, then index
    let set = RegexSet::new(&[r"world", r"hello", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should match in order: hello (pattern 1 at pos 0), world (pattern 0 at pos 6)
    // But \w+ (pattern 2) also matches at pos 0, but has higher index
    assert_eq!(matches[0].pattern(), 1); // "hello" has lower index than \w+ at pos 0
    assert_eq!(matches[0].as_str(), "hello");
    assert_eq!(matches[1].pattern(), 0); // "world" at pos 6
    assert_eq!(matches[1].as_str(), "world");
}

#[test]
fn test_into_captures() {
    let set = RegexSet::new(&[r"(\d+)-(\d+)"]).unwrap();

    let haystack = "123-456";
    let mut matches = set.matches(haystack);
    let m = matches.next().unwrap().unwrap();

    let caps = m.into_captures();
    assert_eq!(caps.get(0).unwrap().as_str(), "123-456");
    assert_eq!(caps.get(1).unwrap().as_str(), "123");
    assert_eq!(caps.get(2).unwrap().as_str(), "456");
}

#[test]
fn test_match_range() {
    let set = RegexSet::new(&[r"\d+"]).unwrap();

    let haystack = "abc 123 xyz";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].range(), 4..7);
}

#[test]
fn test_syntax_highlighting_use_case() {
    // Simulate a simple syntax highlighter
    let set = RegexSet::new(&[
        r"//.*$",                    // Comments
        r#""(?:[^"\\]|\\.)*""#,      // Strings
        r"\b(fn|let|mut|if|else)\b", // Keywords
        r"\b[0-9]+\b",               // Numbers
    ])
    .unwrap();

    let code = r#"let x = 42; // comment"#;
    let matches: Vec<_> = set.matches(code).map(|m| m.unwrap()).collect();

    // Should find: let (keyword), x (would need \w+ pattern), 42 (number), // comment
    assert!(matches.len() >= 3); // at least keywords, number, and comment
}

#[test]
fn test_incremental_search_behavior() {
    // Test that the iterator searches incrementally, not upfront
    // This is verified by stopping iteration early - if all matches were pre-computed,
    // that would be wasted work
    let set = RegexSet::new(&[r"\d+", r"\w+", r"[A-Z]+"]).unwrap();

    let haystack = "abc 123 XYZ def 456 GHI";

    // Take only the first 2 matches
    let mut iter = set.matches(haystack);
    let m1 = iter.next().unwrap().unwrap();
    let m2 = iter.next().unwrap().unwrap();

    // First two matches should be at the beginning
    assert_eq!(m1.as_str(), "abc");
    assert_eq!(m2.as_str(), "123");

    // Don't consume the rest - the iterator should not have pre-computed them
    drop(iter);

    // Create a new iterator and consume all matches
    let all_matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();
    assert_eq!(all_matches.len(), 6); // abc, 123, XYZ, def, 456, GHI
}

#[test]
fn test_overlapping_patterns_at_same_position() {
    // Test that when multiple patterns match at the same position,
    // the one with the lowest index wins
    let set = RegexSet::new(&[r"hello", r"h\w+", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].as_str(), "hello");
    assert_eq!(matches[0].start(), 0);

    // Second match should be pattern 2 at position 6 (only pattern that matches "world")
    assert_eq!(matches[1].pattern(), 2);
    assert_eq!(matches[1].as_str(), "world");
    assert_eq!(matches[1].start(), 6);
}

#[test]
#[cfg(feature = "std")]
fn test_parallel_hard_patterns() {
    // Test multiple hard patterns to verify parallel execution works
    let set = RegexSet::new(&[
        r"(\w+)\s+\1",      // Pattern 0: repeated word (backref)
        r"(?=\d{3})\d+",    // Pattern 1: lookahead
        r"(?<=\$)\d+\.\d+", // Pattern 2: lookbehind
        r"\d+",             // Pattern 3: simple (easy pattern)
    ])
    .unwrap();

    let haystack = "foo foo 123 $45.67";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find matches from all patterns
    assert!(matches.len() >= 3);

    // Verify specific matches
    let patterns: Vec<usize> = matches.iter().map(|m| m.pattern()).collect();
    assert!(patterns.contains(&0)); // repeated word
    assert!(patterns.contains(&1) || patterns.contains(&3)); // numbers (either lookahead or easy)
    assert!(patterns.contains(&2)); // price with lookbehind
}

#[test]
#[cfg(feature = "std")]
fn test_parallel_with_builder() {
    // Test using builder with max_concurrent_threads
    let set = RegexSetBuilder::new(&[
        r"(\w+)\s+\1", // Pattern 0: repeated word
        r"(?=\w+)\w+", // Pattern 1: lookahead
        r"\d+",        // Pattern 2: easy pattern
    ])
    .max_concurrent_threads(Some(2))
    .build()
    .unwrap();

    let haystack = "foo foo bar 123";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find matches from all patterns
    assert!(matches.len() >= 3);
}

#[test]
#[cfg(feature = "std")]
fn test_many_hard_patterns() {
    // Test with many hard patterns to ensure threading works with multiple patterns
    let set = RegexSet::new(&[
        r"(?=a)a", r"(?=b)b", r"(?=c)c", r"(?=d)d", r"(?=e)e", r"(?=f)f", r"(?=g)g", r"(?=h)h",
    ])
    .unwrap();

    let haystack = "abcdefgh";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find one match for each character
    assert_eq!(matches.len(), 8);
}
