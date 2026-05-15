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

fn regex_with_seek(re: &str, seek: bool) -> Regex {
    let parse_result = RegexBuilder::new(re).seek(seek).build();
    assert!(
        parse_result.is_ok(),
        "Expected regex '{}' to be compiled successfully with seek={}, got {:?}",
        re,
        seek,
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

fn ascii_bytes_regex_with_seek(re: &str, seek: bool) -> Regex {
    let parse_result = RegexBuilder::new(re)
        .bytes_mode(BytesMode::Ascii)
        .seek(seek)
        .build();
    assert!(
        parse_result.is_ok(),
        "Expected bytes regex '{}' to be compiled successfully with seek={}, got {:?}",
        re,
        seek,
        parse_result.err()
    );
    parse_result.unwrap()
}

fn capture_spans_str(captures: &Captures<'_, str>) -> Vec<Option<(usize, usize)>> {
    (0..captures.len())
        .map(|i| captures.get(i).map(|m| (m.start(), m.end())))
        .collect()
}

fn capture_spans_bytes(captures: &Captures<'_, [u8]>) -> Vec<Option<(usize, usize)>> {
    (0..captures.len())
        .map(|i| captures.get(i).map(|m| (m.start(), m.end())))
        .collect()
}

fn extract_captures_iter_spans_str(
    captures: &[Captures<'_, str>],
) -> Vec<Vec<Option<(usize, usize)>>> {
    captures.iter().map(capture_spans_str).collect()
}

fn extract_captures_iter_spans_bytes(
    captures: &[Captures<'_, [u8]>],
) -> Vec<Vec<Option<(usize, usize)>>> {
    captures.iter().map(capture_spans_bytes).collect()
}

/// Assert that `re` matches `text` in **both** str mode and ASCII bytes mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_is_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text).unwrap();
    let str_seek_result = regex_with_seek(re, true).is_match(text).unwrap();
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes()).unwrap();
    let bytes_seek_result = ascii_bytes_regex_with_seek(re, true)
        .is_match(text.as_bytes())
        .unwrap();

    assert_eq!(
        str_result, true,
        "Expected regex '{}' to match '{}' (str mode)",
        re, text
    );
    assert_eq!(
        bytes_result,
        true,
        "Expected regex '{}' to match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_result, str_seek_result,
        "Expected regex '{}' to have same str-mode is_match result with/without seek on '{}'",
        re, text
    );
    assert_eq!(
        bytes_result,
        bytes_seek_result,
        "Expected regex '{}' to have same bytes-mode is_match result with/without seek on {:?}",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_seek_result, bytes_seek_result,
        "Expected regex '{}' seek=true is_match results to agree between str and bytes mode for '{}'",
        re, text
    );
}

/// Assert that `re` does **not** match `text` in either str mode or ASCII bytes
/// mode.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_no_match(re: &str, text: &str) {
    let str_result = regex(re).is_match(text).unwrap();
    let str_seek_result = regex_with_seek(re, true).is_match(text).unwrap();
    let bytes_result = ascii_bytes_regex(re).is_match(text.as_bytes()).unwrap();
    let bytes_seek_result = ascii_bytes_regex_with_seek(re, true)
        .is_match(text.as_bytes())
        .unwrap();

    assert_eq!(
        str_result, false,
        "Expected regex '{}' to not match '{}' (str mode)",
        re, text
    );
    assert_eq!(
        bytes_result,
        false,
        "Expected regex '{}' to not match {:?} (bytes mode)",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_result, str_seek_result,
        "Expected regex '{}' to have same str-mode is_match result with/without seek on '{}'",
        re, text
    );
    assert_eq!(
        bytes_result,
        bytes_seek_result,
        "Expected regex '{}' to have same bytes-mode is_match result with/without seek on {:?}",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_seek_result, bytes_seek_result,
        "Expected regex '{}' seek=true is_match results to agree between str and bytes mode for '{}'",
        re, text
    );
}

/// Run `find` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree, and return the common result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_find(re: &str, text: &str) -> Option<(usize, usize)> {
    let str_result = regex(re).find(text).unwrap().map(|m| (m.start(), m.end()));
    let str_seek_result = regex_with_seek(re, true)
        .find(text)
        .unwrap()
        .map(|m| (m.start(), m.end()));
    let bytes_result = ascii_bytes_regex(re)
        .find(text.as_bytes())
        .unwrap()
        .map(|m| (m.start(), m.end()));
    let bytes_seek_result = ascii_bytes_regex_with_seek(re, true)
        .find(text.as_bytes())
        .unwrap()
        .map(|m| (m.start(), m.end()));
    assert_eq!(
        str_result, bytes_result,
        "Expected regex '{}' find results to agree between str and bytes mode for text '{}'",
        re, text
    );
    assert_eq!(
        str_result, str_seek_result,
        "Expected regex '{}' find results to agree in str mode with/without seek for text '{}'",
        re, text
    );
    assert_eq!(
        bytes_result,
        bytes_seek_result,
        "Expected regex '{}' find results to agree in bytes mode with/without seek for text {:?}",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_seek_result, bytes_seek_result,
        "Expected regex '{}' seek=true find results to agree between str and bytes mode for text '{}'",
        re, text
    );
    str_result
}

/// Run `captures_iter` against `text` in both str mode and ASCII bytes mode, assert
/// that both agree on every match's group spans, and return the str-mode results.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures_iter<'t>(re: &str, text: &'t str) -> Vec<Captures<'t, str>> {
    let str_results: Vec<_> = regex(re)
        .captures_iter(text)
        .map(|c| c.expect("captures_iter succeeded (str mode)"))
        .collect();
    let str_seek_results: Vec<_> = regex_with_seek(re, true)
        .captures_iter(text)
        .map(|c| c.expect("captures_iter succeeded (str mode, seek=true)"))
        .collect();
    let bytes_results: Vec<_> = ascii_bytes_regex(re)
        .captures_iter(text.as_bytes())
        .map(|c| c.expect("captures_iter succeeded (bytes mode)"))
        .collect();
    let bytes_seek_results: Vec<_> = ascii_bytes_regex_with_seek(re, true)
        .captures_iter(text.as_bytes())
        .map(|c| c.expect("captures_iter succeeded (bytes mode, seek=true)"))
        .collect();

    let str_spans = extract_captures_iter_spans_str(&str_results);
    let str_seek_spans = extract_captures_iter_spans_str(&str_seek_results);
    let bytes_spans = extract_captures_iter_spans_bytes(&bytes_results);
    let bytes_seek_spans = extract_captures_iter_spans_bytes(&bytes_seek_results);

    assert_eq!(
        str_spans, bytes_spans,
        "Expected regex '{}' captures_iter spans to agree between str and bytes modes for '{}'",
        re, text
    );
    assert_eq!(
        str_spans, str_seek_spans,
        "Expected regex '{}' captures_iter spans to agree in str mode with/without seek for '{}'",
        re, text
    );
    assert_eq!(
        bytes_spans,
        bytes_seek_spans,
        "Expected regex '{}' captures_iter spans to agree in bytes mode with/without seek for {:?}",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_seek_spans, bytes_seek_spans,
        "Expected regex '{}' seek=true captures_iter spans to agree between str and bytes mode for '{}'",
        re, text
    );
    str_results
}

/// Run `captures_from_pos` against `text` starting at `pos` in both str mode and ASCII bytes
/// mode, assert that both agree on every group span, and return the str-mode result.
#[cfg_attr(feature = "track_caller", track_caller)]
#[allow(dead_code)]
pub fn assert_captures_from_pos<'t>(
    re: &str,
    text: &'t str,
    pos: usize,
) -> Option<Captures<'t, str>> {
    let str_result = regex(re)
        .captures_from_pos(text, pos)
        .expect("expected captures_from_pos to succeed (str mode)");
    let str_seek_result = regex_with_seek(re, true)
        .captures_from_pos(text, pos)
        .expect("expected captures_from_pos to succeed (str mode, seek=true)");
    let bytes_result = ascii_bytes_regex(re)
        .captures_from_pos(text.as_bytes(), pos)
        .expect("expected captures_from_pos to succeed (bytes mode)");
    let bytes_seek_result = ascii_bytes_regex_with_seek(re, true)
        .captures_from_pos(text.as_bytes(), pos)
        .expect("expected captures_from_pos to succeed (bytes mode, seek=true)");

    let str_spans = str_result.as_ref().map(capture_spans_str);
    let str_seek_spans = str_seek_result.as_ref().map(capture_spans_str);
    let bytes_spans = bytes_result.as_ref().map(capture_spans_bytes);
    let bytes_seek_spans = bytes_seek_result.as_ref().map(capture_spans_bytes);

    assert_eq!(
        str_spans, bytes_spans,
        "Expected regex '{}' captures_from_pos({}) spans to agree between str and bytes modes for '{}'",
        re, pos, text
    );
    assert_eq!(
        str_spans, str_seek_spans,
        "Expected regex '{}' captures_from_pos({}) spans to agree in str mode with/without seek for '{}'",
        re, pos, text
    );
    assert_eq!(
        bytes_spans, bytes_seek_spans,
        "Expected regex '{}' captures_from_pos({}) spans to agree in bytes mode with/without seek for {:?}",
        re, pos, text.as_bytes()
    );
    assert_eq!(
        str_seek_spans, bytes_seek_spans,
        "Expected regex '{}' seek=true captures_from_pos({}) spans to agree between str and bytes mode for '{}'",
        re, pos, text
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
    let str_seek_result = regex_with_seek(re, true)
        .captures(text)
        .expect("expected captures to succeed (str mode, seek=true)");
    let bytes_result = ascii_bytes_regex(re)
        .captures(text.as_bytes())
        .expect("expected captures to succeed (bytes mode)");
    let bytes_seek_result = ascii_bytes_regex_with_seek(re, true)
        .captures(text.as_bytes())
        .expect("expected captures to succeed (bytes mode, seek=true)");

    let str_spans = str_result.as_ref().map(capture_spans_str);
    let str_seek_spans = str_seek_result.as_ref().map(capture_spans_str);
    let bytes_spans = bytes_result.as_ref().map(capture_spans_bytes);
    let bytes_seek_spans = bytes_seek_result.as_ref().map(capture_spans_bytes);

    assert_eq!(
        str_spans, bytes_spans,
        "Expected regex '{}' captures spans to agree between str and bytes mode for '{}'",
        re, text
    );
    assert_eq!(
        str_spans, str_seek_spans,
        "Expected regex '{}' captures spans to agree in str mode with/without seek for '{}'",
        re, text
    );
    assert_eq!(
        bytes_spans,
        bytes_seek_spans,
        "Expected regex '{}' captures spans to agree in bytes mode with/without seek for {:?}",
        re,
        text.as_bytes()
    );
    assert_eq!(
        str_seek_spans, bytes_seek_spans,
        "Expected regex '{}' seek=true captures spans to agree between str and bytes mode for '{}'",
        re, text
    );
    str_result
}

use std::fmt;
#[allow(dead_code)]
pub struct DebugRegex<'a>(pub &'a Regex);
impl fmt::Display for DebugRegex<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.debug_print(f)
    }
}
