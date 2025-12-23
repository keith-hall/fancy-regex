use regex_automata::{nfa::thompson, dfa::dense, hybrid};

fn main() {
    // Test pattern without word boundaries
    let pattern_no_wb = r"(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing hybrid DFA WITHOUT word boundaries: {}", pattern_no_wb);
    let result = hybrid::dfa::DFA::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern_no_wb);
    
    match result {
        Ok(_) => println!("Hybrid DFA: SUCCESS"),
        Err(e) => println!("Hybrid DFA: FAILED - {}", e),
    }
    
    println!("\nTesting dense DFA WITHOUT word boundaries: {}", pattern_no_wb);
    let result2 = dense::DFA::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern_no_wb);
    
    match result2 {
        Ok(_) => println!("Dense DFA: SUCCESS"),
        Err(e) => println!("Dense DFA: FAILED - {}", e),
    }
    
    // Now test with word boundaries
    let pattern_with_wb = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("\nTesting hybrid DFA WITH word boundaries: {}", pattern_with_wb);
    let result3 = hybrid::dfa::DFA::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern_with_wb);
    
    match result3 {
        Ok(_) => println!("Hybrid DFA: SUCCESS"),
        Err(e) => println!("Hybrid DFA: FAILED - {}", e),
    }
}
