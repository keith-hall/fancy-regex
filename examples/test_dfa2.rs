use regex_automata::{dfa::dense, hybrid, nfa::thompson, util::syntax};

fn main() {
    // Test if dense DFA supports word boundaries with heuristic enabling
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing dense DFA with heuristic word boundary support...");
    let result = dense::DFA::builder()
        .syntax(syntax::Config::new().unicode_word_boundary(true))
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result {
        Ok(_) => println!("Dense DFA: SUCCESS - supports word boundaries with unicode enabled"),
        Err(e) => println!("Dense DFA: FAILED - {}", e),
    }
    
    println!("\nTesting hybrid DFA with heuristic word boundary support...");
    let result2 = hybrid::dfa::DFA::builder()
        .syntax(syntax::Config::new().unicode_word_boundary(true))
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result2 {
        Ok(_) => println!("Hybrid DFA: SUCCESS - supports word boundaries with unicode enabled"),
        Err(e) => println!("Hybrid DFA: FAILED - {}", e),
    }
}
