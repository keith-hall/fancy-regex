use regex_automata::{dfa::dense, hybrid, nfa::thompson};

fn main() {
    // Test if dense DFA supports word boundaries
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing dense DFA with pattern: {}", pattern);
    let result = dense::DFA::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result {
        Ok(_) => println!("Dense DFA: SUCCESS - supports word boundaries"),
        Err(e) => println!("Dense DFA: FAILED - {}", e),
    }
    
    println!("\nTesting hybrid DFA with pattern: {}", pattern);
    let result2 = hybrid::dfa::DFA::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result2 {
        Ok(_) => println!("Hybrid DFA: SUCCESS - supports word boundaries"),
        Err(e) => println!("Hybrid DFA: FAILED - {}", e),
    }
}
