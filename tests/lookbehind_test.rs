use fancy_regex::Regex;

#[test]
fn test_variable_lookbehind_patterns_compile() {
    // These should now work with the reverse lookbehind implementation
    let result1 = Regex::new(r"(?<=a+b+)");
    println!("Pattern (?<=a+b+): {:?}", result1);
    assert!(result1.is_ok());
    
    let result2 = Regex::new(r"(?<=a(?:b|cd))");
    println!("Pattern (?<=a(?:b|cd)): {:?}", result2);
    assert!(result2.is_ok()); 
    
    // Test negative lookbehinds too
    let result3 = Regex::new(r"(?<!a+b+)");
    println!("Pattern (?<!a+b+): {:?}", result3);
    assert!(result3.is_ok());
    
    let result4 = Regex::new(r"(?<!a(?:b|cd))");
    println!("Pattern (?<!a(?:b|cd)): {:?}", result4);
    assert!(result4.is_ok());
    
    // This should still work (alternation with const sizes)
    let result5 = Regex::new(r"(?<=a|bc)");
    println!("Pattern (?<=a|bc): {:?}", result5);
    assert!(result5.is_ok());
}

#[test]
fn test_variable_lookbehind_functionality() {
    // Test (?<=a+b+) pattern  
    let re = Regex::new(r"(?<=a+b+)x").unwrap();
    assert!(re.is_match("abx").unwrap());
    assert!(re.is_match("aabbx").unwrap());
    assert!(re.is_match("aaabbbx").unwrap());
    assert!(!re.is_match("ax").unwrap());
    assert!(!re.is_match("bx").unwrap());
    assert!(!re.is_match("abcx").unwrap());
    
    // Test (?<=a(?:b|cd)) pattern
    let re2 = Regex::new(r"(?<=a(?:b|cd))x").unwrap();
    assert!(re2.is_match("abx").unwrap());
    assert!(re2.is_match("acdx").unwrap());
    assert!(!re2.is_match("ax").unwrap());
    assert!(!re2.is_match("bcx").unwrap());
}