// Copyright 2016 The Fancy Regex Authors.
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

//! Tests for state deduplication optimization that prevents catastrophic backtracking.

use fancy_regex::{Regex, RegexBuilder};
use std::time::{Duration, Instant};

#[test]
fn catastrophic_backtracking_pattern_with_backrefs() {
    // Pattern known for catastrophic backtracking: (a+)(a+)\1\2
    // Without state deduplication, this would take exponential time
    let re = Regex::new(r"(a+)(a+)\1\2").unwrap();
    
    // Test with a string of 30 'a's
    let text = "a".repeat(30);
    
    let start = Instant::now();
    let result = re.is_match(&text);
    let duration = start.elapsed();
    
    // Should complete quickly (under 1 second) thanks to state deduplication
    assert!(duration < Duration::from_secs(1), 
            "Regex took too long: {:?}", duration);
    assert!(result.is_ok());
}

#[test]
fn catastrophic_backtracking_alternation_pattern() {
    // Pattern (a|b|ab)*bc without a 'c' in the input causes exponential backtracking
    // State deduplication should make this manageable
    let re = RegexBuilder::new(r"(a|b|ab)*bc")
        .backtrack_limit(100_000)
        .build()
        .unwrap();
    
    // String that doesn't match (ends with 'ac' not 'bc')
    let text = "ab".repeat(25) + "ac";
    
    let start = Instant::now();
    let result = re.is_match(&text);
    let duration = start.elapsed();
    
    // Should complete quickly thanks to state deduplication
    assert!(duration < Duration::from_secs(1), 
            "Regex took too long: {:?}", duration);
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn catastrophic_backtracking_nested_quantifiers() {
    // Pattern with nested quantifiers that can cause catastrophic backtracking
    let re = RegexBuilder::new(r"(a+)+(b+)+(c+)+")
        .backtrack_limit(50_000)
        .build()
        .unwrap();
    
    // String that doesn't match (no 'c')
    let text = "a".repeat(15) + &"b".repeat(15);
    
    let start = Instant::now();
    let result = re.is_match(&text);
    let duration = start.elapsed();
    
    // Should complete quickly thanks to state deduplication
    assert!(duration < Duration::from_secs(1), 
            "Regex took too long: {:?}", duration);
    // This should not error due to state deduplication
    assert!(result.is_ok());
}

#[test]
fn state_deduplication_allows_reasonable_matches() {
    // Verify that state deduplication doesn't break normal matching
    let re = Regex::new(r"(a+)(a+)\1\2").unwrap();
    
    // This should match: "aaa" + "a" + "aaa" + "a" = "aaaaaaaa"
    assert!(re.is_match("aaaaaaaa").unwrap());
    
    // This should also match (finds "aaaaaaaa" at the start)
    assert!(re.is_match("aaaaaaaab").unwrap());
    
    // This should not match
    assert!(!re.is_match("aaa").unwrap());
}

#[test]
fn state_deduplication_with_captures() {
    // Verify that captures work correctly with state deduplication
    let re = Regex::new(r"(a+)(a+)\1\2").unwrap();
    
    let text = "aaaaaaaa";
    let caps = re.captures(text).unwrap();
    
    assert!(caps.is_some());
    let caps = caps.unwrap();
    assert_eq!(caps.get(0).map(|m| m.as_str()), Some("aaaaaaaa"));
    assert_eq!(caps.get(1).map(|m| m.as_str()), Some("aaa"));
    assert_eq!(caps.get(2).map(|m| m.as_str()), Some("a"));
}

#[test]
fn state_deduplication_performance_comparison() {
    // Compare performance with and without excessive backtracking
    let pattern = r"(?i)(a|b|ab)*(?>c)";
    
    // With a low backtrack limit, this should error
    let re_low_limit = RegexBuilder::new(pattern)
        .backtrack_limit(1_000)
        .build()
        .unwrap();
    
    let text = "ab".repeat(27);
    let result_low = re_low_limit.is_match(&text);
    assert!(result_low.is_err(), "Expected backtrack limit error with low limit");
    
    // With a higher limit, state deduplication should make it succeed
    let re_high_limit = RegexBuilder::new(pattern)
        .backtrack_limit(10_000)
        .build()
        .unwrap();
    
    let start = Instant::now();
    let result_high = re_high_limit.is_match(&text);
    let duration = start.elapsed();
    
    assert!(duration < Duration::from_secs(1), 
            "Regex took too long: {:?}", duration);
    assert!(result_high.is_ok(), "Expected success due to state deduplication");
    assert!(!result_high.unwrap(), "Expected no match");
}
