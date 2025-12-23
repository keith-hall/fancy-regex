use regex_automata::{hybrid, dfa::dense, nfa::thompson};

fn main() {
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing hybrid DFA with unicode_word_boundary enabled...");
    let result = hybrid::dfa::DFA::builder()
        .configure(hybrid::dfa::Config::new().unicode_word_boundary(true))
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result {
        Ok(_) => println!("Hybrid DFA: SUCCESS"),
        Err(e) => println!("Hybrid DFA: FAILED - {}", e),
    }
    
    println!("\nTesting dense DFA with unicode_word_boundary enabled...");
    let result2 = dense::DFA::builder()
        .configure(dense::Config::new().unicode_word_boundary(true))
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern);
    
    match result2 {
        Ok(_) => println!("Dense DFA: SUCCESS"),
        Err(e) => println!("Dense DFA: FAILED - {}", e),
    }
}
