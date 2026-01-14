//! Tests for \R escape sequence (Unicode newline)

use fancy_regex::Regex;

#[test]
fn test_r_escape_matches_crlf() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\r\n").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 2);
    assert_eq!(m.as_str(), "\r\n");
}

#[test]
fn test_r_escape_matches_lf() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\n").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 1);
    assert_eq!(m.as_str(), "\n");
}

#[test]
fn test_r_escape_matches_cr() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\r").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 1);
    assert_eq!(m.as_str(), "\r");
}

#[test]
fn test_r_escape_matches_vt() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\x0b").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 1);
    assert_eq!(m.as_str(), "\x0b");
}

#[test]
fn test_r_escape_matches_ff() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\x0c").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 1);
    assert_eq!(m.as_str(), "\x0c");
}

#[test]
fn test_r_escape_matches_nel() {
    let re = Regex::new(r"\R").unwrap();
    let m = re.find("\u{0085}").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 2); // NEL is 2 bytes in UTF-8
    assert_eq!(m.as_str(), "\u{0085}");
}

#[test]
fn test_r_escape_no_backtracking() {
    // This is the critical test: \R\n should NOT match \r\n
    // because \R matches \r\n atomically and doesn't backtrack to just \r
    let re = Regex::new(r"\R\n").unwrap();
    let result = re.find("\r\n").unwrap();
    assert!(
        result.is_none(),
        "\\R\\n should not match \\r\\n due to atomic matching"
    );
}

#[test]
fn test_r_escape_with_following_pattern() {
    // \R\n should match when there's \n followed by another \n
    let re = Regex::new(r"\R\n").unwrap();

    // Test with \n followed by another \n
    let m = re.find("a\n\nb").unwrap().unwrap();
    assert_eq!(m.start(), 1);
    assert_eq!(m.end(), 3);
    assert_eq!(m.as_str(), "\n\n");
}

#[test]
fn test_r_escape_in_oniguruma_mode() {
    let re = fancy_regex::RegexBuilder::new(r"\R")
        .oniguruma_mode(true)
        .build()
        .unwrap();

    let m = re.find("\r\n").unwrap().unwrap();
    assert_eq!(m.start(), 0);
    assert_eq!(m.end(), 2);
}
