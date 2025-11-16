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

//! Tests for position tracking in the parser and analyzer

use fancy_regex::internal::{analyze, Info};
use fancy_regex::Expr;

/// Helper to check positions are correctly tracked through parsing and analysis
fn check_positions(pattern: &str, expected_positions: &[(usize, &str)]) {
    let tree = Expr::parse_tree(pattern).expect("Failed to parse");
    let info = analyze(&tree, false).expect("Failed to analyze");

    // Collect all positions by traversing the Info tree
    let mut actual_positions = Vec::new();
    collect_positions(&info, &mut actual_positions);

    assert_eq!(
        actual_positions.len(),
        expected_positions.len(),
        "Mismatch in number of tracked positions for pattern '{}': expected {}, got {}",
        pattern,
        expected_positions.len(),
        actual_positions.len()
    );

    for (i, (expected_pos, expected_desc)) in expected_positions.iter().enumerate() {
        let actual_pos = actual_positions[i];
        assert_eq!(
            actual_pos, *expected_pos,
            "Position mismatch for {} in pattern '{}': expected {}, got {}",
            expected_desc, pattern, expected_pos, actual_pos
        );
    }
}

fn collect_positions(info: &Info, positions: &mut Vec<usize>) {
    positions.push(info.start_ix);
    for child in &info.children {
        collect_positions(child, positions);
    }
}

#[test]
fn test_simple_literal() {
    // Pattern: "abc"
    // Order: 'a' at 0, 'b' at 1, 'c' at 2, Concat at 0
    check_positions(
        "abc",
        &[
            (0, "Concat"),
            (0, "Literal 'a'"),
            (1, "Literal 'b'"),
            (2, "Literal 'c'"),
        ],
    );
}

#[test]
fn test_anchors_and_dot() {
    // Pattern: "^\s\b."
    // Per the problem statement: anchor at 0, whitespace at 1, word boundary at 3, dot at 5
    // Order: Concat, then each child in tree order
    check_positions(
        r"^\s\b.",
        &[
            (0, "Concat"),
            (0, "Assertion ^"),
            (1, "Delegate \\s"),
            (3, "Assertion \\b"),
            (5, "Any ."),
        ],
    );
}

#[test]
fn test_alternation() {
    // Pattern: "a|b|c"
    // Alt at 0, 'a' at 0, 'b' at 2, 'c' at 4
    check_positions(
        "a|b|c",
        &[
            (0, "Alt"),
            (0, "Literal 'a'"),
            (2, "Literal 'b'"),
            (4, "Literal 'c'"),
        ],
    );
}

#[test]
fn test_group() {
    // Pattern: "(abc)"
    // Group at 0, Concat inside
    check_positions(
        "(abc)",
        &[
            (0, "Group"),
            (1, "Concat"),
            (1, "Literal 'a'"),
            (2, "Literal 'b'"),
            (3, "Literal 'c'"),
        ],
    );
}

#[test]
fn test_repeat() {
    // Pattern: "a+"
    // Repeat at 0, wrapping Literal 'a' at 0
    check_positions("a+", &[(0, "Repeat"), (0, "Literal 'a'")]);
}

#[test]
fn test_repeat_with_quantifier() {
    // Pattern: "a{2,3}"
    check_positions("a{2,3}", &[(0, "Repeat"), (0, "Literal 'a'")]);
}

#[test]
fn test_lookahead() {
    // Pattern: "a(?=b)"
    check_positions(
        "a(?=b)",
        &[
            (0, "Concat"),
            (0, "Literal 'a'"),
            (1, "LookAround"),
            (4, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_lookbehind() {
    // Pattern: "(?<=a)b"
    check_positions(
        "(?<=a)b",
        &[
            (0, "Concat"),
            (0, "LookAround"),
            (4, "Literal 'a'"),
            (6, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_negative_lookahead() {
    // Pattern: "a(?!b)"
    check_positions(
        "a(?!b)",
        &[
            (0, "Concat"),
            (0, "Literal 'a'"),
            (1, "LookAround"),
            (4, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_negative_lookbehind() {
    // Pattern: "(?<!a)b"
    check_positions(
        "(?<!a)b",
        &[
            (0, "Concat"),
            (0, "LookAround"),
            (5, "Literal 'a'"),
            (7, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_backref() {
    // Pattern: "(a)\\1"
    check_positions(
        r"(a)\1",
        &[
            (0, "Concat"),
            (0, "Group"),
            (1, "Literal 'a'"),
            (3, "Backref \\1"),
        ],
    );
}

#[test]
fn test_named_group() {
    // Pattern: "(?<name>a)\\k<name>"
    check_positions(
        r"(?<name>a)\k<name>",
        &[
            (0, "Concat"),
            (0, "Group"),
            (8, "Literal 'a'"),
            (10, "Backref \\k<name>"),
        ],
    );
}

#[test]
fn test_character_class() {
    // Pattern: "[abc]"
    check_positions("[abc]", &[(0, "Delegate [abc]")]);
}

#[test]
fn test_atomic_group() {
    // Pattern: "(?>ab)"
    check_positions(
        "(?>ab)",
        &[
            (0, "AtomicGroup"),
            (3, "Concat"),
            (3, "Literal 'a'"),
            (4, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_possessive_quantifier() {
    // Pattern: "a++", which becomes AtomicGroup(Repeat)
    check_positions(
        "a++",
        &[(0, "AtomicGroup"), (0, "Repeat"), (0, "Literal 'a'")],
    );
}

#[test]
fn test_escape_sequences() {
    // Pattern: "\\n\\t\\r"
    check_positions(
        r"\n\t\r",
        &[
            (0, "Concat"),
            (0, "Literal \\n"),
            (2, "Literal \\t"),
            (4, "Literal \\r"),
        ],
    );
}

#[test]
fn test_hex_escape() {
    // Pattern: "\\x41" (ASCII 'A')
    check_positions(r"\x41", &[(0, "Literal \\x41")]);
}

#[test]
fn test_unicode_escape() {
    // Pattern: "\\u0041"
    check_positions(r"\u0041", &[(0, "Literal \\u0041")]);
}

#[test]
fn test_word_boundary() {
    // Pattern: "\\bword\\b"
    check_positions(
        r"\bword\b",
        &[
            (0, "Concat"),
            (0, "Assertion \\b"),
            (2, "Concat"),
            (2, "Literal 'w'"),
            (3, "Literal 'o'"),
            (4, "Literal 'r'"),
            (5, "Literal 'd'"),
            (6, "Assertion \\b"),
        ],
    );
}

#[test]
fn test_start_end_assertions() {
    // Pattern: "\\Atext\\z"
    check_positions(
        r"\Atext\z",
        &[
            (0, "Concat"),
            (0, "Assertion \\A"),
            (2, "Concat"),
            (2, "Literal 't'"),
            (3, "Literal 'e'"),
            (4, "Literal 'x'"),
            (5, "Literal 't'"),
            (6, "Assertion \\z"),
        ],
    );
}

#[test]
fn test_delegate_patterns() {
    // Pattern: "\\d+\\w*\\s?"
    check_positions(
        r"\d+\w*\s?",
        &[
            (0, "Concat"),
            (0, "Repeat"),
            (0, "Delegate \\d"),
            (3, "Repeat"),
            (3, "Delegate \\w"),
            (6, "Repeat"),
            (6, "Delegate \\s"),
        ],
    );
}

#[test]
fn test_empty_expression() {
    // Pattern: "" (empty)
    check_positions("", &[(0, "Empty")]);
}

#[test]
fn test_flag_group() {
    // Pattern: "(?i)abc" - flags change but produce no expr
    check_positions(
        "(?i)abc",
        &[
            (0, "Concat"),
            (0, "Literal 'a'"),
            (5, "Literal 'b'"),
            (6, "Literal 'c'"),
        ],
    );
}

#[test]
fn test_flag_scoped_group() {
    // Pattern: "(?i:abc)"
    check_positions(
        "(?i:abc)",
        &[
            (1, "Concat"),
            (4, "Literal 'a'"),
            (5, "Literal 'b'"),
            (6, "Literal 'c'"),
        ],
    );
}

#[test]
fn test_conditional() {
    // Pattern: "(?(1)a|b)"
    check_positions(
        "(?(1)a|b)",
        &[
            (0, "Conditional"),
            (0, "Backref (1)"),
            (5, "Literal 'a'"),
            (7, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_backref_exists_condition() {
    // Pattern: "(h)?(?(1))"
    check_positions(
        "(h)?(?(1))",
        &[
            (0, "Concat"),
            (0, "Repeat"),
            (0, "Group"),
            (1, "Literal 'h'"),
            (4, "BackrefExistsCondition"),
        ],
    );
}

#[test]
fn test_keepout() {
    // Pattern: "a\\Kb"
    check_positions(
        r"a\Kb",
        &[
            (0, "Concat"),
            (0, "Literal 'a'"),
            (1, "KeepOut"),
            (3, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_continue_from_previous() {
    // Pattern: "\\Gtest"
    check_positions(
        r"\Gtest",
        &[
            (0, "Concat"),
            (0, "ContinueFromPreviousMatchEnd"),
            (2, "Concat"),
            (2, "Literal 't'"),
            (3, "Literal 'e'"),
            (4, "Literal 's'"),
            (5, "Literal 't'"),
        ],
    );
}

#[test]
fn test_complex_pattern() {
    // Pattern: "^(\\d{3})-(\\d{2})-\\d{4}$"
    // A complex pattern with multiple features
    check_positions(
        r"^(\d{3})-(\d{2})-\d{4}$",
        &[
            (0, "Concat"),
            (0, "Assertion ^"),
            (1, "Group"),
            (2, "Repeat"),
            (2, "Delegate \\d"),
            (8, "Literal '-'"),
            (9, "Group"),
            (10, "Repeat"),
            (10, "Delegate \\d"),
            (16, "Literal '-'"),
            (17, "Repeat"),
            (17, "Delegate \\d"),
            (23, "Assertion $"),
        ],
    );
}

#[test]
fn test_nested_groups() {
    // Pattern: "((a)(b))"
    check_positions(
        "((a)(b))",
        &[
            (0, "Group"),
            (1, "Concat"),
            (1, "Group"),
            (2, "Literal 'a'"),
            (4, "Group"),
            (5, "Literal 'b'"),
        ],
    );
}

#[test]
fn test_alternation_in_group() {
    // Pattern: "(a|b|c)"
    check_positions(
        "(a|b|c)",
        &[
            (0, "Group"),
            (1, "Alt"),
            (1, "Literal 'a'"),
            (3, "Literal 'b'"),
            (5, "Literal 'c'"),
        ],
    );
}
