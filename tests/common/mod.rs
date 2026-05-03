use fancy_regex::{BytesMode, Regex, RegexBuilder};

#[allow(dead_code)]
pub fn regex(re: &str) -> Regex {
    let parse_result = Regex::new(re);
    assert!(
        parse_result.is_ok(),
        "Expected regex '{}' to be compiled successfully, got {:?}",
        re,
        parse_result.err()
    );
    parse_result.unwrap()
}

/// Build a regex in ASCII bytes mode.  Useful for bytes-mode tests that need
/// to configure options beyond what the simple dual-mode helpers provide.
#[allow(dead_code)]
pub fn bytes_regex(re: &str) -> Regex {
    let parse_result = RegexBuilder::new(re).bytes_mode(BytesMode::Ascii).build();
    assert!(
        parse_result.is_ok(),
        "Expected bytes regex '{}' to be compiled successfully, got {:?}",
        re,
        parse_result.err()
    );
    parse_result.unwrap()
}

/// Assert that `re` matches `text` in **both** str mode and ASCII bytes mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_is_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text);
    assert_eq!(
        str_result.unwrap(),
        true,
        "Expected regex '{}' to match '{}' (str mode)",
        re,
        text
    );
    let bytes_result = bytes_regex(re).is_match(text.as_bytes());
    assert_eq!(
        bytes_result.unwrap(),
        true,
        "Expected regex '{}' to match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
}

/// Assert that `re` does **not** match `text` in either str mode or ASCII bytes
/// mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_no_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text);
    assert_eq!(
        str_result.unwrap(),
        false,
        "Expected regex '{}' to not match '{}' (str mode)",
        re,
        text
    );
    let bytes_result = bytes_regex(re).is_match(text.as_bytes());
    assert_eq!(
        bytes_result.unwrap(),
        false,
        "Expected regex '{}' to not match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
}

/// Run `find` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree, and return the common result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_find(re: &str, text: &str) -> Option<(usize, usize)> {
    let str_result = regex(re).find(text).unwrap().map(|m| (m.start(), m.end()));
    let bytes_result = bytes_regex(re)
        .find(text.as_bytes())
        .unwrap()
        .map(|m| (m.start(), m.end()));
    assert_eq!(
        str_result, bytes_result,
        "Expected regex '{}' find results to agree between str and bytes mode for text '{}'",
        re, text
    );
    str_result
}
