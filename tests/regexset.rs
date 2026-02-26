mod common;

use fancy_regex::{Error, RegexOptionsBuilder, RegexSet, RuntimeError};

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
fn test_priority_resolution_easy_patterns() {
    // When multiple patterns match at the same position, lowest index wins
    let set = RegexSet::new(&[r"hello", r"h\w+", r"\w+"]).unwrap();

    assert_eq!(set.len(), 3);
    assert!(!set.is_empty());

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 2);

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].as_str(), "hello");

    assert_eq!(matches[1].pattern(), 2);
    assert_eq!(matches[1].as_str(), "world");
}

#[test]
fn test_hard_patterns_mixed_with_easy_patterns() {
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
fn test_priority_resolution_hard_patterns() {
    // When multiple patterns match at the same position, lowest index wins
    let set = RegexSet::new(&[r"(\w+)\s+\1", r"(?=hello)\w+\s+\w+"]).unwrap();

    assert_eq!(set.len(), 2);
    assert!(!set.is_empty());

    let haystack = "hello hello";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 1);

    // First match should be pattern 0 at position 0 (lowest index)
    assert_eq!(matches[0].pattern(), 0);
    assert_eq!(matches[0].start(), 0);
}

#[test]
fn test_zero_width_matches() {
    // Test that zero-width matches don't cause infinite loops
    let set = RegexSet::new(&[r"\b", r"\w+"]).unwrap();

    let haystack = "hello world";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    // Should find word boundaries and words, but not loop infinitely
    assert_eq!(matches.len(), 6);
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
fn test_no_matches() {
    let set = RegexSet::new(&[r"\d+", r"[A-Z]+"]).unwrap();

    let haystack = "abc";
    let matches: Vec<_> = set.matches(haystack).map(|m| m.unwrap()).collect();

    assert_eq!(matches.len(), 0);
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
fn test_zero_width_matches_utf8_boundary() {
    // Test that zero-width matches with multibyte UTF-8 characters don't cause issues
    let set = RegexSet::new(&[r"\d*(?=é)", r"é"]).unwrap();

    let text = "é1é";
    let matches: Vec<_> = set.matches(text).map(|m| m.unwrap()).collect();

    // Should find zero-width matches and the é characters
    // Verify we got matches at expected positions
    let positions: Vec<_> = matches.iter().map(|m| m.start()).collect();
    assert!(positions.contains(&0), "Should have match at position 0");
    assert!(positions.contains(&2), "Should have match at position 2");
    assert!(positions.contains(&3), "Should have match at position 3");
}

#[test]
fn test_backtrack_limit_error_handling() {
    // Test that when a backtrack limit is hit, the iterator stops properly
    let mut options = RegexOptionsBuilder::new();
    options.backtrack_limit(1);
    let set = RegexSet::new_with_options(&[r"(x+x+)+(?>y)", r"\d+"], &options)
        .expect("all regex patterns should compile successfully");

    let text = "xxxxxxxxxxy 123";
    let result: Vec<_> = set.matches(text).collect();

    // Should get an error for the first pattern that exceeds the backtrack limit
    assert_eq!(result.len(), 1);
    assert!(result[0].is_err());
    match &result[0].as_ref().err() {
        Some(Error::RuntimeError(RuntimeError::BacktrackLimitExceeded)) => {}
        _ => panic!("Expected RuntimeError::BacktrackLimitExceeded"),
    }
}

#[test]
fn test_zero_width_match_multibyte_char() {
    // Test zero-width matches with emoji (4-byte UTF-8)
    // This test ensures that after a zero-width match at a multibyte character boundary,
    // the iterator correctly advances to the next UTF-8 codepoint boundary
    let set = RegexSet::new(&[r"(?=🎯)", r"[a-z]+"]).unwrap();

    let text = "foo🎯bar";
    let matches: Vec<_> = set.matches(text).map(|m| m.unwrap()).collect();

    // Should find "foo" (ASCII letters), zero-width before emoji, and "bar" (ASCII letters)
    assert_eq!(matches.len(), 3);

    // Verify the matches
    assert_eq!(matches[0].as_str(), "foo");
    assert_eq!(matches[0].pattern(), 1);

    // Zero-width match before emoji (at byte position 3, which is a 4-byte UTF-8 boundary)
    assert_eq!(matches[1].as_str(), "");
    assert_eq!(matches[1].pattern(), 0);
    assert_eq!(matches[1].start(), 3); // Position after "foo"

    // After advancing past the zero-width match and the 4-byte emoji,
    // we should correctly find "bar" at byte position 7
    assert_eq!(matches[2].as_str(), "bar");
    assert_eq!(matches[2].pattern(), 1);
    assert_eq!(matches[2].start(), 7); // Position after "foo" + emoji (3 + 4 bytes)
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
fn test_single_pattern_equivalence_with_find_iter() {
    // A single-pattern RegexSet must produce the same matches as Regex::find_iter.
    use fancy_regex::Regex;
    let pattern = r"(\d)\d";
    let text = "11 22 33";

    let regex = Regex::new(pattern).unwrap();
    let standalone: Vec<_> = regex.find_iter(text).map(|m| m.unwrap().range()).collect();

    let set = RegexSet::new(&[pattern]).unwrap();
    let set_matches: Vec<_> = set.matches(text).map(|m| m.unwrap().range()).collect();

    assert_eq!(standalone, set_matches);
}

#[test]
fn test_single_pattern_equivalence_zero_width() {
    // An easy single-pattern RegexSet must match find_iter even for zero-width matches.
    use fancy_regex::Regex;
    let pattern = r"\d*(?=[a-z])";
    let text = "ab1c2";

    let regex = Regex::new(pattern).unwrap();
    let standalone: Vec<_> = regex.find_iter(text).map(|m| m.unwrap().range()).collect();

    let set = RegexSet::new(&[pattern]).unwrap();
    let set_matches: Vec<_> = set.matches(text).map(|m| m.unwrap().range()).collect();

    assert_eq!(standalone, set_matches);
}

#[test]
fn test_single_hard_pattern_g_anchor_non_empty() {
    // \G with a non-empty match: RegexSet must behave like find_iter for a hard pattern
    // that only matches at the continuation point.
    use fancy_regex::Regex;
    let pattern = r"\G(\d)\d";
    let text = "1122 33";

    let regex = Regex::new(pattern).unwrap();
    let standalone: Vec<_> = regex.find_iter(text).map(|m| m.unwrap().range()).collect();

    let set = RegexSet::new(&[pattern]).unwrap();
    let set_matches: Vec<_> = set.matches(text).map(|m| m.unwrap().range()).collect();

    // Standalone produces two consecutive matches at 0..2 and 2..4 then stops.
    assert_eq!(standalone, vec![0..2, 2..4]);
    assert_eq!(standalone, set_matches);
}

#[test]
fn test_single_hard_pattern_g_anchor_allows_empty() {
    // \G\d* can produce an empty match immediately after a non-empty one.
    // The per-pattern skip (mirroring standalone) must suppress that empty match.
    use fancy_regex::Regex;
    let pattern = r"\G\d*";
    let text = "1122 33";

    let regex = Regex::new(pattern).unwrap();
    let standalone: Vec<_> = regex.find_iter(text).map(|m| m.unwrap().range()).collect();

    let set = RegexSet::new(&[pattern]).unwrap();
    let set_matches: Vec<_> = set.matches(text).map(|m| m.unwrap().range()).collect();

    // Both should give exactly one match: "1122" at 0..4.
    assert_eq!(standalone, vec![0..4]);
    assert_eq!(standalone, set_matches);
}

#[test]
fn test_g_anchor_in_regexset_differs_from_standalone() {
    // Document the known difference: inside a RegexSet the \G continuation
    // point is the *global* iterator position (end of whatever matched last),
    // not the end of the last match of that specific pattern.
    //
    // Pattern 0 (\G\d+) and Pattern 1 (\w+) together: Pattern 0 matches "123"
    // at position 0, then Pattern 1 matches "abc" at 3, advancing the iterator
    // to position 6.  \G then successfully matches "456" at position 6 even
    // though standalone \G\d+ would have stopped after "123".
    use fancy_regex::Regex;
    let text = "123abc456";

    let set = RegexSet::new(&[r"\G\d+", r"[a-z]+"]).unwrap();
    let set_matches: Vec<_> = set.matches(text).map(|m| m.unwrap()).collect();

    assert_eq!(set_matches.len(), 3);
    assert_eq!(set_matches[0].as_str(), "123");
    assert_eq!(set_matches[0].pattern(), 0);
    assert_eq!(set_matches[1].as_str(), "abc");
    assert_eq!(set_matches[1].pattern(), 1);
    assert_eq!(set_matches[2].as_str(), "456");
    assert_eq!(set_matches[2].pattern(), 0);

    // In contrast, standalone \G\d+ only produces "123".
    let standalone: Vec<_> = Regex::new(r"\G\d+")
        .unwrap()
        .find_iter(text)
        .map(|m| m.unwrap().as_str().to_string())
        .collect();
    assert_eq!(standalone, vec!["123"]);
}
