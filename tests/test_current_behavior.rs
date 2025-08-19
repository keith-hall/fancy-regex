#[cfg(test)]
mod test_lookbehind_backref {
    use fancy_regex::Regex;
    
    #[test]
    fn test_current_behavior() {
        println!("Testing variable-size lookbehind with backref:");
        // This should now work with the fixed to_str method
        let result = Regex::new(r"(..)(?<=\1)");
        match result {
            Ok(regex) => {
                println!("Regex compiled successfully!");
                // Test with some sample input
                match regex.find("abab") {
                    Ok(Some(m)) => println!("Match found: {:?}", m),
                    Ok(None) => println!("No match found in 'abab'"),
                    Err(e) => println!("Error during matching: {}", e),
                }
            }
            Err(e) => println!("Error compiling: {}", e),
        }
        
        println!("\nTesting constant-size lookbehind with backref:");
        // Test constant-size lookbehind with backref 
        let result2 = Regex::new(r"(a)(?<=\1)");
        match result2 {
            Ok(regex) => {
                println!("Constant-size regex compiled successfully!");
                match regex.find("aa") {
                    Ok(Some(m)) => println!("Match found: {:?}", m),
                    Ok(None) => println!("No match found in 'aa'"),
                    Err(e) => println!("Error during matching: {}", e),
                }
            }
            Err(e) => println!("Error compiling constant-size: {}", e),
        }
    }
}