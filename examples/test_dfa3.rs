use regex_automata::{dfa::dense, hybrid, nfa::thompson};

fn main() {
    // Test if dense DFA supports word boundaries with NFA config
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing dense DFA with utf8=false...");
    let result = dense::DFA::builder()
        .thompson(thompson::Config::new().reverse(true).utf8(false))
        .build(pattern);
    
    match result {
        Ok(_) => println!("Dense DFA: SUCCESS"),
        Err(e) => println!("Dense DFA: FAILED - {}", e),
    }
    
    println!("\nTesting with NFA pikevm...");
    let pattern2 = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    let result3 = regex_automata::nfa::thompson::pikevm::PikeVM::builder()
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern2);
    
    match result3 {
        Ok(_) => println!("PikeVM: SUCCESS - supports word boundaries"),
        Err(e) => println!("PikeVM: FAILED - {}", e),
    }
}
