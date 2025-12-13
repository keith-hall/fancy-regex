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

//! Tests for catastrophic backtracking prevention via state deduplication

use fancy_regex::Regex;

/// This test demonstrates that the state deduplication optimization prevents
/// catastrophic backtracking for a classic pathological regex pattern.
///
/// The pattern `(a+)+b` with input "aaaa...aaac" (many 'a's followed by 'c')
/// would normally cause exponential time complexity due to the nested quantifiers.
/// With state deduplication, duplicate (pc, ix) states are skipped, preventing
/// the exponential explosion.
#[test]
fn test_nested_quantifiers_no_match() {
    // This would cause catastrophic backtracking without optimization
    let re = Regex::new(r"(a+)+b").unwrap();
    let input = "a".repeat(25) + "c";
    
    // This should complete quickly thanks to state deduplication
    let result = re.is_match(&input);
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

/// Test that state deduplication still finds matches correctly
#[test]
fn test_nested_quantifiers_with_match() {
    let re = Regex::new(r"(a+)+b").unwrap();
    let input = "a".repeat(25) + "b";
    
    let result = re.is_match(&input);
    assert!(result.is_ok());
    assert!(result.unwrap());
}

/// Test with backreferences that would cause catastrophic backtracking
#[test]
fn test_backref_catastrophic_pattern() {
    // Pattern with nested quantifiers and backref
    let re = Regex::new(r"^(a+)+\1$").unwrap();
    
    // This input doesn't match but would cause exponential backtracking
    // Using a smaller input size to avoid hitting the backtrack limit
    let input = "a".repeat(15) + "b";
    
    let result = re.is_match(&input);
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

/// Test that we still find matches with backref patterns
#[test]
fn test_backref_finds_match() {
    let re = Regex::new(r"(a+)+\1").unwrap();
    
    // This should match: "aaa" + "aaa"
    let result = re.is_match("aaaaaa");
    assert!(result.is_ok());
    assert!(result.unwrap());
}

/// Test alternation with overlap that can cause catastrophic backtracking
#[test]
fn test_overlapping_alternation() {
    let re = Regex::new(r"(a|a)*b").unwrap();
    let input = "a".repeat(25) + "c";
    
    let result = re.is_match(&input);
    assert!(result.is_ok());
    assert!(!result.unwrap());
}
