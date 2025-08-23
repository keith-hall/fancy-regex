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

/// Serializable representation of position information
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SerializableSpan {
    pub start: usize,
    pub end: usize,
}

/// Serializable representation of an expression node for the parse tree
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SerializableExpr {
    pub node_type: String,
    pub span: Option<SerializableSpan>,
    pub children: Vec<SerializableExpr>,
    pub details: serde_json::Value,
}

/// Serializable representation of analysis information
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SerializableAnalysisInfo {
    pub node_type: String,
    pub span: Option<SerializableSpan>,
    pub is_hard: bool,
    pub min_size: usize,
    pub const_size: bool,
    pub children: Vec<SerializableAnalysisInfo>,
    pub details: serde_json::Value,
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
fn build_regex(pattern: &str, flags: &RegexFlags) -> Result<Regex, JsValue> {
    let mut builder = RegexBuilder::new(pattern);

    builder.case_insensitive(flags.case_insensitive);
    builder.multi_line(flags.multi_line);
    builder.dot_matches_new_line(flags.dot_matches_new_line);
    builder.ignore_whitespace(flags.ignore_whitespace);
    builder.unicode_mode(flags.unicode);

    builder
        .build()
        .map_err(|e| JsValue::from_str(&format!("Regex compilation error: {}", e)))
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

/// Convert an Expr to a serializable format with position tracking
fn expr_to_serializable(expr: &fancy_regex::Expr, start_pos: usize, pattern: &str) -> SerializableExpr {
    use fancy_regex::Expr;
    
    // Calculate approximate span (this is simplified - ideally we'd track positions during parsing)
    let span = Some(SerializableSpan {
        start: start_pos,
        end: (start_pos + 1).min(pattern.len()), // Simplified - in real implementation would be precise
    });

    let (node_type, children, details) = match expr {
        Expr::Empty => ("Empty".to_string(), vec![], serde_json::Value::Null),
        Expr::Any { newline } => ("Any".to_string(), vec![], serde_json::json!({ "newline": newline })),
        Expr::Assertion(assertion) => ("Assertion".to_string(), vec![], serde_json::json!({ "assertion": format!("{:?}", assertion) })),
        Expr::Literal { val, casei } => ("Literal".to_string(), vec![], serde_json::json!({ "value": val, "case_insensitive": casei })),
        Expr::Concat(exprs) => {
            let children: Vec<SerializableExpr> = exprs.iter().enumerate()
                .map(|(i, e)| expr_to_serializable(e, start_pos + i, pattern))
                .collect();
            ("Concat".to_string(), children, serde_json::Value::Null)
        },
        Expr::Alt(exprs) => {
            let children: Vec<SerializableExpr> = exprs.iter().enumerate()
                .map(|(i, e)| expr_to_serializable(e, start_pos + i, pattern))
                .collect();
            ("Alt".to_string(), children, serde_json::Value::Null)
        },
        Expr::Group(expr) => {
            let child = expr_to_serializable(expr, start_pos + 1, pattern);
            ("Group".to_string(), vec![child], serde_json::Value::Null)
        },
        Expr::LookAround(expr, la) => {
            let child = expr_to_serializable(expr, start_pos + 2, pattern); // Account for (?
            ("LookAround".to_string(), vec![child], serde_json::json!({ "look_around": format!("{:?}", la) }))
        },
        Expr::Repeat { child, lo, hi, greedy } => {
            let child_expr = expr_to_serializable(child, start_pos, pattern);
            ("Repeat".to_string(), vec![child_expr], serde_json::json!({ "lo": lo, "hi": hi, "greedy": greedy }))
        },
        Expr::Delegate { inner, size, casei } => {
            ("Delegate".to_string(), vec![], serde_json::json!({ "inner": inner, "size": size, "case_insensitive": casei }))
        },
        Expr::Backref { group, casei } => {
            ("Backref".to_string(), vec![], serde_json::json!({ "group": group, "case_insensitive": casei }))
        },
        Expr::BackrefWithRelativeRecursionLevel { group, relative_level, casei } => {
            ("BackrefWithRelativeRecursionLevel".to_string(), vec![], serde_json::json!({ "group": group, "relative_level": relative_level, "case_insensitive": casei }))
        },
        Expr::AtomicGroup(expr) => {
            let child = expr_to_serializable(expr, start_pos + 3, pattern); // Account for (?>
            ("AtomicGroup".to_string(), vec![child], serde_json::Value::Null)
        },
        Expr::KeepOut => ("KeepOut".to_string(), vec![], serde_json::Value::Null),
        Expr::ContinueFromPreviousMatchEnd => ("ContinueFromPreviousMatchEnd".to_string(), vec![], serde_json::Value::Null),
        Expr::BackrefExistsCondition(group) => ("BackrefExistsCondition".to_string(), vec![], serde_json::json!({ "group": group })),
        Expr::Conditional { condition, true_branch, false_branch } => {
            let cond_child = expr_to_serializable(condition, start_pos + 2, pattern);
            let true_child = expr_to_serializable(true_branch, start_pos + 3, pattern);
            let false_child = expr_to_serializable(false_branch, start_pos + 4, pattern);
            ("Conditional".to_string(), vec![cond_child, true_child, false_child], serde_json::Value::Null)
        },
        Expr::SubroutineCall(group) => ("SubroutineCall".to_string(), vec![], serde_json::json!({ "group": group })),
        Expr::UnresolvedNamedSubroutineCall { name, ix } => {
            ("UnresolvedNamedSubroutineCall".to_string(), vec![], serde_json::json!({ "name": name, "position": ix }))
        },
    };

    SerializableExpr {
        node_type,
        span,
        children,
        details,
    }
}

/// Convert analysis Info to serializable format  
fn analysis_to_serializable(info: &fancy_regex::internal::Info, pattern: &str) -> SerializableAnalysisInfo {
    // Extract the necessary information from the Info struct
    let span = Some(SerializableSpan {
        start: 0, // Simplified - would need to track actual positions
        end: 1,
    });

    let node_type = match info.expr {
        fancy_regex::Expr::Empty => "Empty",
        fancy_regex::Expr::Any { .. } => "Any",
        fancy_regex::Expr::Assertion(_) => "Assertion",
        fancy_regex::Expr::Literal { .. } => "Literal",
        fancy_regex::Expr::Concat(_) => "Concat",
        fancy_regex::Expr::Alt(_) => "Alt",
        fancy_regex::Expr::Group(_) => "Group",
        fancy_regex::Expr::LookAround(_, _) => "LookAround",
        fancy_regex::Expr::Repeat { .. } => "Repeat",
        fancy_regex::Expr::Delegate { .. } => "Delegate",
        fancy_regex::Expr::Backref { .. } => "Backref",
        fancy_regex::Expr::BackrefWithRelativeRecursionLevel { .. } => "BackrefWithRelativeRecursionLevel",
        fancy_regex::Expr::AtomicGroup(_) => "AtomicGroup",
        fancy_regex::Expr::KeepOut => "KeepOut",
        fancy_regex::Expr::ContinueFromPreviousMatchEnd => "ContinueFromPreviousMatchEnd",
        fancy_regex::Expr::BackrefExistsCondition(_) => "BackrefExistsCondition",
        fancy_regex::Expr::Conditional { .. } => "Conditional",
        fancy_regex::Expr::SubroutineCall(_) => "SubroutineCall",
        fancy_regex::Expr::UnresolvedNamedSubroutineCall { .. } => "UnresolvedNamedSubroutineCall",
    };

    let children: Vec<SerializableAnalysisInfo> = info.children.iter()
        .map(|child| analysis_to_serializable(child, pattern))
        .collect();

    SerializableAnalysisInfo {
        node_type: node_type.to_string(),
        span,
        is_hard: info.hard,
        min_size: info.min_size,
        const_size: info.const_size,
        children,
        details: serde_json::json!({
            "start_group": info.start_group,
            "end_group": info.end_group
        }),
    }
}


#[wasm_bindgen]
pub fn find_captures(pattern: &str, text: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags)?;

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
pub fn parse_regex(pattern: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(tree) => {
            let serializable = expr_to_serializable(&tree.expr, 0, pattern);
            serde_wasm_bindgen::to_value(&serializable)
                .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
        },
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn analyze_regex(pattern: &str, flags: JsValue) -> Result<JsValue, JsValue> {
    let flags = get_flags(flags)?;
    let regex_flags = compute_regex_flags(&flags);

    use fancy_regex::internal::{analyze, optimize};

    match fancy_regex::Expr::parse_tree_with_flags(pattern, regex_flags) {
        Ok(mut tree) => {
            optimize(&mut tree);
            match analyze(&tree, 1) {
                Ok(info) => {
                    let serializable = analysis_to_serializable(&info, pattern);
                    serde_wasm_bindgen::to_value(&serializable)
                        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
                },
                Err(e) => Err(JsValue::from_str(&format!("Analysis error: {}", e))),
            }
        }
        Err(e) => Err(JsValue::from_str(&format!("Parse error: {}", e))),
    }
}

#[wasm_bindgen]
pub fn is_match(pattern: &str, text: &str, flags: JsValue) -> Result<bool, JsValue> {
    let flags = get_flags(flags)?;
    let regex = build_regex(pattern, &flags)?;

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
