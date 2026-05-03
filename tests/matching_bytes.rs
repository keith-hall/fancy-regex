use fancy_regex::{BytesMode, RegexBuilder};

#[test]
fn bytes_find_from_pos() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find_from_pos(b"abc 123", 4).unwrap().unwrap();
    assert_eq!(mat.start(), 4);
    assert_eq!(mat.end(), 7);
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_find_from_pos_no_match() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.find_from_pos(b"abc 123", 7).unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_non_utf8_input() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let input = b"\x80\x81\x82 123";
    assert!(re.is_match(input).unwrap());

    let mat = re.find_from_pos(input, 0).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_ascii_dot_matches_non_utf8() {
    let re = RegexBuilder::new(r".+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"\x80\x81\x82").unwrap());

    let re = RegexBuilder::new(r".")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"\xff").unwrap());
    assert!(re.is_match(b"\x80").unwrap());

    let re = RegexBuilder::new(r".*")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find_from_pos(b"\xff\xfe\xfd", 0).unwrap().unwrap();
    assert_eq!(mat.as_bytes(), b"\xff\xfe\xfd");
}

#[test]
fn bytes_unicode_bytes_dot_does_not_match_raw_bytes() {
    let re = RegexBuilder::new(r".")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(!re.is_match(b"\x80").unwrap());
    assert!(re.is_match(b"A").unwrap());
}

#[test]
fn bytes_unicode_bytes_char_classes_still_unicode() {
    let re = RegexBuilder::new(r"\w+")
        .bytes_mode(BytesMode::UnicodeBytes)
        .build()
        .unwrap();
    assert!(re.is_match("café".as_bytes()).unwrap());
}

#[test]
fn bytes_ascii_char_classes_are_ascii_only() {
    let re = RegexBuilder::new(r"^\w+$")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    assert!(re.is_match(b"hello").unwrap());
    assert!(!re.is_match("café".as_bytes()).unwrap());
}

#[test]
fn bytes_find_returns_matchbytes() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find(b"abc 123").unwrap().unwrap();
    assert_eq!(mat.start(), 4);
    assert_eq!(mat.end(), 7);
    assert_eq!(mat.as_bytes(), b"123");
}

#[test]
fn bytes_find_no_match() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let result = re.find(b"abc").unwrap();
    assert!(result.is_none());
}

#[test]
fn bytes_find_iter() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mut matches = re.find_iter(b"a1 b23 c456");
    let m1 = matches.next().unwrap().unwrap();
    assert_eq!(m1.as_bytes(), b"1");
    let m2 = matches.next().unwrap().unwrap();
    assert_eq!(m2.as_bytes(), b"23");
    let m3 = matches.next().unwrap().unwrap();
    assert_eq!(m3.as_bytes(), b"456");
    assert!(matches.next().is_none());
}

#[test]
fn bytes_find_iter_non_utf8() {
    let re = RegexBuilder::new(r".+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mut matches = re.find_iter(b"\x80\x81\x82");
    let m = matches.next().unwrap().unwrap();
    assert_eq!(m.as_bytes(), b"\x80\x81\x82");
    assert!(matches.next().is_none());
}

#[test]
fn bytes_find_with_str_still_works() {
    let re = RegexBuilder::new(r"\d+")
        .bytes_mode(BytesMode::Ascii)
        .build()
        .unwrap();
    let mat = re.find("abc 123").unwrap().unwrap();
    assert_eq!(mat.as_str(), "123");
}
