use fancy_regex::Regex;

fn main() {
    println!("Testing pattern with {{0}} repetition...\n");
    
    // Test 1: The original reported issue
    let pattern = r"(?<=)(abc)(ABC){0}";
    let text = "abc";
    println!("Pattern: {}", pattern);
    println!("Text: {}", text);
    let regex = Regex::new(pattern).unwrap();
    match regex.is_match(text) {
        Ok(matched) => println!("Result: {}", matched),
        Err(e) => println!("Error: {:?}", e),
    }
    println!();
    
    // Test 2: Without lookbehind (should still go through VM if there's {0})
    let pattern2 = r"(abc)(ABC){0}";
    let text2 = "abc";
    println!("Pattern: {}", pattern2);
    println!("Text: {}", text2);
    let regex2 = Regex::new(pattern2).unwrap();
    match regex2.is_match(text2) {
        Ok(matched) => println!("Result: {}", matched),
        Err(e) => println!("Error: {:?}", e),
    }
    println!();
    
    // Test 3: With captures
    let pattern3 = r"(?<=)(abc)(ABC){0}";
    let text3 = "abc";
    println!("Pattern: {}", pattern3);
    println!("Text: {}", text3);
    let regex3 = Regex::new(pattern3).unwrap();
    match regex3.captures(text3) {
        Ok(Some(caps)) => {
            println!("Matched!");
            for (i, cap) in caps.iter().enumerate() {
                if let Some(m) = cap {
                    println!("  Group {}: {:?}", i, m.as_str());
                } else {
                    println!("  Group {}: None", i);
                }
            }
        },
        Ok(None) => println!("No match"),
        Err(e) => println!("Error: {:?}", e),
    }
}
