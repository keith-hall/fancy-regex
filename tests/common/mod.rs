use fancy_regex::{BytesMode, Captures, Regex, RegexBuilder};

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
pub fn ascii_bytes_regex(re: &str) -> Regex {
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
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes());
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
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes());
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
    let bytes_result = ascii_bytes_regex(re)
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

/// Run `captures` against `text` in both str mode and ASCII bytes mode, assert
/// that both engines agree on the spans of every capture group, and return the
/// str-mode `Captures` (or `None` if neither engine matched).
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures<'t>(re: &str, text: &'t str) -> Option<Captures<'t, str>> {
    let str_result = regex(re)
        .captures(text)
        .expect("expected captures to succeed (str mode)");
    let bytes_result = ascii_bytes_regex(re)
        .captures(text.as_bytes())
        .expect("expected captures to succeed (bytes mode)");
    assert_eq!(
        str_result.is_some(),
        bytes_result.is_some(),
        "Expected regex '{}' captures to agree between str and bytes mode for '{}'",
        re,
        text
    );
    if let (Some(ref s), Some(ref b)) = (&str_result, &bytes_result) {
        assert_eq!(
            s.len(),
            b.len(),
            "Expected capture group count to agree for regex '{}' on '{}'",
            re,
            text
        );
        for i in 0..s.len() {
            let str_span = s.get(i).map(|m| (m.start(), m.end()));
            let bytes_span = b.get(i).map(|m| (m.start(), m.end()));
            assert_eq!(
                str_span,
                bytes_span,
                "Expected capture group {} to agree between str and bytes mode for regex '{}' on '{}'",
                i,
                re,
                text
            );
        }
    }
    str_result
}
