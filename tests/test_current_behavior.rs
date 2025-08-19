#[cfg(test)]
mod test_lookbehind_backref {
    use fancy_regex::Regex;
    
    #[test]
    fn test_current_behavior() {
        // This should currently fail with an error
        let result = Regex::new(r"(..)(?<=\1)");
        match result {
            Ok(_) => println!("Unexpectedly succeeded!"),
            Err(e) => println!("Error as expected: {}", e),
        }
    }
}