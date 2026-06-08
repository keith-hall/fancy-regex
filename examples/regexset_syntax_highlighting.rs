// Example demonstrating RegexSet usage for syntax highlighting

use fancy_regex::{RegexInput, RegexOptionsBuilder, RegexSet, Result};

#[derive(Clone, Debug, PartialEq, Eq)]
struct Token {
    pattern: usize,
    token_type: &'static str,
    start: usize,
    end: usize,
    text: String,
}

fn token_type_for_pattern(pattern: usize) -> &'static str {
    match pattern {
        0 => "comment",
        1 => "string",
        2 => "keyword",
        3 => "number",
        4 => "identifier",
        _ => "unknown",
    }
}

fn build_syntax_set() -> Result<RegexSet> {
    let mut options_builder = RegexOptionsBuilder::new();
    options_builder.multi_line(true);
    RegexSet::new_with_options(
        &[
            r"//.*$",                    // 0: Single-line comments
            r#""(?:[^"\\]|\\.)*""#,      // 1: String literals
            r"\b(fn|let|mut|if|else)\b", // 2: Keywords
            r"\b[0-9]+\b",               // 3: Numbers
            r"[a-zA-Z_][a-zA-Z0-9_]*",   // 4: Identifiers
        ],
        &options_builder,
    )
}

fn tokenize_code(code: &str, set: &RegexSet) -> Result<Vec<Token>> {
    let mut start = 0;
    let mut tokens = Vec::new();

    while start < code.len() {
        if let Some(mut matching_patterns) =
            set.find_input(RegexInput::new(code).from_pos(start))?
        {
            if let Some(match_result) = matching_patterns.next() {
                let m = match_result?;
                tokens.push(Token {
                    pattern: m.pattern(),
                    token_type: token_type_for_pattern(m.pattern()),
                    start: m.start(),
                    end: m.end(),
                    text: m.as_str().to_owned(),
                });
                start = m.end();
                continue;
            }
        }
        break;
    }

    Ok(tokens)
}

fn main() -> Result<()> {
    // Create a regex set for basic syntax highlighting
    let set = build_syntax_set()?;

    let code = r#"let x = 42; // a comment
let s = "hello world";"#;

    println!("Tokenizing code:\n{}\n", code);
    println!("Tokens found:");

    for token in tokenize_code(code, &set)? {
        println!(
            "  [{}] {} at {}..{}: '{}'",
            token.pattern, token.token_type, token.start, token.end, token.text
        );
    }

    Ok(())
}
