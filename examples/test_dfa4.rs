use regex_automata::{meta::Regex, Input, Anchored};

fn main() {
    // Test if meta regex supports word boundaries in reverse
    let pattern = r"\b(?:[A-Z][a-z]*|[a-z]+)";
    
    println!("Testing meta Regex with pattern: {}", pattern);
    let result = Regex::builder()
        .build(pattern);
    
    match result {
        Ok(re) => {
            println!("Meta Regex: SUCCESS - built successfully");
            
            // Test reverse search
            let text = "Carefuly";
            let input = Input::new(text)
                .anchored(Anchored::Yes)
                .range(0..7);  // Search from position 7 backwards
            
            println!("Testing reverse search on '{}'", text);
            // Meta regex doesn't directly expose reverse search in the same way
            // but we can test if it works normally
            if let Some(m) = re.find(text) {
                println!("Found match: {:?}", m);
            }
        }
        Err(e) => println!("Meta Regex: FAILED - {}", e),
    }
}
