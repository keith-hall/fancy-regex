use regex_automata::{hybrid, nfa::thompson, Input, Anchored};

fn main() {
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Building reverse DFA with unicode_word_boundary enabled...");
    let dfa = hybrid::dfa::DFA::builder()
        .configure(hybrid::dfa::Config::new().unicode_word_boundary(true))
        .thompson(thompson::Config::new().reverse(true))
        .build(pattern)
        .unwrap();
    
    let mut cache = dfa.create_cache();
    
    // Test reverse search on "Carefuly" - should find "fuly" when searching backwards from position 11
    let text = "Carefuly";
    let input = Input::new(text)
        .span(0..11)  // Search in reverse from position 11 to 0
        .anchored(Anchored::Yes);
    
    println!("Testing reverse search on '{}'", text);
    match dfa.try_search_rev(&mut cache, &input) {
        Ok(Some(m)) => println!("Found match: {:?} ({}..{})", &text[m.range()], m.start(), m.end()),
        Ok(None) => println!("No match found"),
        Err(e) => println!("Error: {}", e),
    }
}
