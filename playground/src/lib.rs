use fancy_regex::internal::{FLAG_CASEI, FLAG_DOTNL, FLAG_IGNORE_SPACE, FLAG_MULTI, FLAG_UNICODE};
use fancy_regex::{Regex, RegexBuilder};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// Expose console.log for debugging
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

// Macro for console.log
macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[derive(Serialize, Deserialize)]
pub struct Match {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

#[derive(Serialize, Deserialize)]
pub struct CaptureGroup {
    pub index: usize,
    pub name: Option<String>,
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub text: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct MatchResult {
    pub full_match: Option<Match>,
    pub captures: Vec<CaptureGroup>,
}

#[derive(Serialize, Deserialize)]
pub struct RegexFlags {
    pub case_insensitive: bool,
    pub multi_line: bool,
    pub dot_matches_new_line: bool,
    pub ignore_whitespace: bool,
    pub unicode: bool,
}

impl Default for RegexFlags {
    fn default() -> Self {
        Self {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: true,
        }
    }
}

// Helper function to deserialize flags or use default
fn get_flags(flags: JsValue) -> Result<RegexFlags, JsValue> {
    if flags.is_undefined() {
        Ok(RegexFlags::default())
    } else {
        serde_wasm_bindgen::from_value(flags)
            .map_err(|e| JsValue::from_str(&format!("Invalid flags: {}", e)))
    }
}

// Helper function to build regex with flags
fn build_regex(pattern: &str, flags: &RegexFlags) -> Result<Regex, String> {
    let mut builder = RegexBuilder::new(pattern);

    builder.case_insensitive(flags.case_insensitive);
    builder.multi_line(flags.multi_line);
    builder.dot_matches_new_line(flags.dot_matches_new_line);
    builder.ignore_whitespace(flags.ignore_whitespace);
    builder.unicode_mode(flags.unicode);

    builder
        .build()
        .map_err(|e| format!("Regex compilation error: {}", e))
}

// Helper function to compute regex flags for parse_tree_with_flags
fn compute_regex_flags(flags: &RegexFlags) -> u32 {
    let mut result = 0;
    if flags.case_insensitive {
        result |= FLAG_CASEI;
    }
    if flags.multi_line {
        result |= FLAG_MULTI;
    }
    if flags.dot_matches_new_line {
        result |= FLAG_DOTNL;
    }
    if flags.ignore_whitespace {
        result |= FLAG_IGNORE_SPACE;
    }
    if flags.unicode {
        result |= FLAG_UNICODE;
    }
    result
}

#[wasm_bindgen]
pub fn find_matches(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags).map_err(|e| JsValue::from_str(&e))?;

    let mut matches = Vec::new();
    for mat in regex.find_iter(text) {
        match mat {
            Ok(m) => {
                matches.push(Match {
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str().to_string(),
                });
            }
            Err(e) => return Err(JsValue::from_str(&format!("Match error: {}", e))),
        }
    }

    serde_wasm_bindgen::to_value(&matches)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn find_captures(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags).map_err(|e| JsValue::from_str(&e))?;

    // Get capture group names once for this regex
    let capture_names: Vec<Option<&str>> = regex.capture_names().collect();

    let mut all_captures = Vec::new();

    for caps_result in regex.captures_iter(text) {
        match caps_result {
            Ok(caps) => {
                let full_match = caps.get(0).map(|m| Match {
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str().to_string(),
                });

                let mut captures = Vec::new();
                for i in 0..caps.len() {
                    let name = capture_names
                        .get(i)
                        .and_then(|&name_opt| name_opt.map(|s| s.to_string()));
                    let capture = if let Some(m) = caps.get(i) {
                        CaptureGroup {
                            index: i,
                            name,
                            start: Some(m.start()),
                            end: Some(m.end()),
                            text: Some(m.as_str().to_string()),
                        }
                    } else {
                        CaptureGroup {
                            index: i,
                            name,
                            start: None,
                            end: None,
                            text: None,
                        }
                    };
                    captures.push(capture);
                }

                all_captures.push(MatchResult {
                    full_match,
                    captures,
                });
            }
            Err(e) => return Err(JsValue::from_str(&format!("Capture error: {}", e))),
        }
    }

    serde_wasm_bindgen::to_value(&all_captures)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn parse_regex(pattern: &str, flags: JsValue) -> Result<String, JsValue> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(tree) => Ok(format!("{:#?}", tree)),
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn analyze_regex(pattern: &str, flags: JsValue) -> Result<String, JsValue> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    use fancy_regex::internal::{analyze, optimize};

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(mut tree) => {
            let requires_capture_group_fixup = optimize(&mut tree);
            match analyze(&tree, if requires_capture_group_fixup { 0 } else { 1 }) {
                Ok(info) => Ok(format!("{:#?}", info)),
                Err(e) => Err(JsValue::from_str(&format!("Analysis error: {}", e))),
            }
        }
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn is_match(pattern: &str, text: &str, flags: JsValue) -> Result<bool, JsValue> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags).map_err(|e| JsValue::from_str(&e))?;

    match regex.is_match(text) {
        Ok(result) => Ok(result),
        Err(e) => Err(JsValue::from_str(&format!("Match error: {}", e))),
    }
}

// Initialize the module
#[wasm_bindgen(start)]
pub fn main() {
    console_log!("fancy-regex WASM playground initialized");
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_regex_flags_default() {
        let flags = RegexFlags::default();
        assert!(!flags.case_insensitive);
        assert!(!flags.multi_line);
        assert!(!flags.dot_matches_new_line);
        assert!(!flags.ignore_whitespace);
        assert!(flags.unicode);
    }

    #[test]
    fn test_build_regex_simple() {
        let flags = RegexFlags::default();
        let regex = build_regex(r"\d+", &flags);
        assert!(regex.is_ok());
    }

    #[test]
    fn test_build_regex_with_flags() {
        let flags = RegexFlags {
            case_insensitive: true,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: true,
        };
        let regex = build_regex(r"hello", &flags);
        assert!(regex.is_ok());
        
        // Test that the regex actually respects case insensitive flag
        let regex = regex.unwrap();
        assert!(regex.is_match("HELLO").unwrap());
        assert!(regex.is_match("hello").unwrap());
    }

    #[test]
    fn test_build_regex_multiline() {
        let flags = RegexFlags {
            case_insensitive: false,
            multi_line: true,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: true,
        };
        let regex = build_regex(r"^test$", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        assert!(regex.is_match("line1\ntest\nline3").unwrap());
    }

    #[test]
    fn test_build_regex_dot_matches_newline() {
        let flags = RegexFlags {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: true,
            ignore_whitespace: false,
            unicode: true,
        };
        let regex = build_regex(r"a.b", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        assert!(regex.is_match("a\nb").unwrap());
    }

    #[test]
    fn test_build_regex_ignore_whitespace() {
        let flags = RegexFlags {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: true,
            unicode: true,
        };
        let regex = build_regex(r"a b c", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        assert!(regex.is_match("abc").unwrap());
    }

    #[test]
    fn test_build_regex_invalid_pattern() {
        let flags = RegexFlags::default();
        let result = build_regex(r"[", &flags);
        assert!(result.is_err());
        let error_msg = format!("{:?}", result.unwrap_err());
        assert!(error_msg.contains("Regex compilation error"));
    }

    #[test]
    fn test_compute_regex_flags_none() {
        let flags = RegexFlags {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: false,
        };
        let computed = compute_regex_flags(&flags);
        assert_eq!(computed, 0);
    }

    #[test]
    fn test_compute_regex_flags_all() {
        let flags = RegexFlags {
            case_insensitive: true,
            multi_line: true,
            dot_matches_new_line: true,
            ignore_whitespace: true,
            unicode: true,
        };
        let computed = compute_regex_flags(&flags);
        
        // Should have all flags set
        assert_ne!(computed & FLAG_CASEI, 0);
        assert_ne!(computed & FLAG_MULTI, 0);
        assert_ne!(computed & FLAG_DOTNL, 0);
        assert_ne!(computed & FLAG_IGNORE_SPACE, 0);
        assert_ne!(computed & FLAG_UNICODE, 0);
    }

    #[test]
    fn test_compute_regex_flags_individual() {
        // Test case insensitive only
        let flags = RegexFlags {
            case_insensitive: true,
            multi_line: false,
            dot_matches_new_line: false,
            ignore_whitespace: false,
            unicode: false,
        };
        let computed = compute_regex_flags(&flags);
        assert_ne!(computed & FLAG_CASEI, 0);
        assert_eq!(computed & FLAG_MULTI, 0);
        assert_eq!(computed & FLAG_DOTNL, 0);
        assert_eq!(computed & FLAG_IGNORE_SPACE, 0);
        assert_eq!(computed & FLAG_UNICODE, 0);
    }

    #[test]
    fn test_match_struct() {
        let match_obj = Match {
            start: 0,
            end: 5,
            text: "hello".to_string(),
        };
        assert_eq!(match_obj.start, 0);
        assert_eq!(match_obj.end, 5);
        assert_eq!(match_obj.text, "hello");
    }

    #[test]
    fn test_capture_group_struct() {
        let capture = CaptureGroup {
            index: 1,
            name: Some("test".to_string()),
            start: Some(0),
            end: Some(5),
            text: Some("hello".to_string()),
        };
        assert_eq!(capture.index, 1);
        assert_eq!(capture.name, Some("test".to_string()));
        assert_eq!(capture.start, Some(0));
        assert_eq!(capture.end, Some(5));
        assert_eq!(capture.text, Some("hello".to_string()));
    }

    #[test]
    fn test_match_result_struct() {
        let full_match = Match {
            start: 0,
            end: 5,
            text: "hello".to_string(),
        };
        let capture = CaptureGroup {
            index: 0,
            name: None,
            start: Some(0),
            end: Some(5),
            text: Some("hello".to_string()),
        };
        let result = MatchResult {
            full_match: Some(full_match),
            captures: vec![capture],
        };
        assert!(result.full_match.is_some());
        assert_eq!(result.captures.len(), 1);
    }

    #[test]
    fn test_regex_with_fancy_features() {
        // Test with lookahead
        let flags = RegexFlags::default();
        let regex = build_regex(r"(?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{8,}", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        assert!(regex.is_match("Password123").unwrap());
        assert!(!regex.is_match("password").unwrap());
    }

    #[test]
    fn test_regex_with_backreference() {
        let flags = RegexFlags::default();
        let regex = build_regex(r"(\w+)\s+\1", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        assert!(regex.is_match("hello hello").unwrap());
        assert!(!regex.is_match("hello world").unwrap());
    }

    #[test]
    fn test_regex_with_named_groups() {
        let flags = RegexFlags::default();
        let regex = build_regex(r"(?P<word>\w+)", &flags);
        assert!(regex.is_ok());
        
        let regex = regex.unwrap();
        let caps = regex.captures("hello").unwrap().unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "hello");
    }

    #[test]
    fn test_complex_pattern_parsing() {
        let flags = RegexFlags::default();
        let regex_flags = compute_regex_flags(&flags);
        
        // Test that complex patterns can be parsed
        let result = fancy_regex::Expr::parse_tree_with_flags(r"(?P<num>\d+)", regex_flags);
        assert!(result.is_ok());
        
        let result = fancy_regex::Expr::parse_tree_with_flags(r"(?=.*test)", regex_flags);
        assert!(result.is_ok());
        
        let result = fancy_regex::Expr::parse_tree_with_flags(r"(\w+)\s+\1", regex_flags);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_pattern_parsing() {
        let flags = RegexFlags::default();
        let regex_flags = compute_regex_flags(&flags);
        
        // Test that invalid patterns are properly rejected
        let result = fancy_regex::Expr::parse_tree_with_flags(r"[", regex_flags);
        assert!(result.is_err());
        
        let result = fancy_regex::Expr::parse_tree_with_flags(r"(?P<>)", regex_flags);
        assert!(result.is_err());
    }

    #[test]
    fn test_regex_analysis() {
        let flags = RegexFlags::default();
        let regex_flags = compute_regex_flags(&flags);
        
        use fancy_regex::internal::{analyze, optimize};
        
        // Test analysis of a simple pattern
        let result = fancy_regex::Expr::parse_tree_with_flags(r"\d+", regex_flags);
        assert!(result.is_ok());
        
        let mut tree = result.unwrap();
        let requires_capture_group_fixup = optimize(&mut tree);
        let analysis = analyze(&tree, if requires_capture_group_fixup { 0 } else { 1 });
        assert!(analysis.is_ok());
    }

    #[test]
    fn test_serialization_structures() {
        // Test that our structures can be serialized/deserialized
        use serde_json;
        
        let match_obj = Match {
            start: 0,
            end: 5,
            text: "hello".to_string(),
        };
        let json = serde_json::to_string(&match_obj).unwrap();
        let deserialized: Match = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.start, match_obj.start);
        assert_eq!(deserialized.end, match_obj.end);
        assert_eq!(deserialized.text, match_obj.text);
    }

    #[test]
    fn test_flag_combinations() {
        // Test various flag combinations work
        let test_cases = vec![
            (true, false, false, false, true),   // case insensitive + unicode
            (false, true, true, false, true),    // multiline + dot newline + unicode
            (true, true, true, true, true),      // all flags
            (false, false, false, true, false), // only ignore whitespace
        ];
        
        for (ci, ml, dn, iw, u) in test_cases {
            let flags = RegexFlags {
                case_insensitive: ci,
                multi_line: ml,
                dot_matches_new_line: dn,
                ignore_whitespace: iw,
                unicode: u,
            };
            
            // Test that we can build a regex with these flags
            let result = build_regex(r"test", &flags);
            assert!(result.is_ok());
            
            // Test that flag computation works
            let computed = compute_regex_flags(&flags);
            if ci { assert_ne!(computed & FLAG_CASEI, 0); }
            if ml { assert_ne!(computed & FLAG_MULTI, 0); }
            if dn { assert_ne!(computed & FLAG_DOTNL, 0); }
            if iw { assert_ne!(computed & FLAG_IGNORE_SPACE, 0); }
            if u { assert_ne!(computed & FLAG_UNICODE, 0); }
        }
    }
}
