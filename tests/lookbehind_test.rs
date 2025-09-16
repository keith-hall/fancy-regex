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
    // Test (?<=a+b+) positive pattern  
    let re = Regex::new(r"(?<=a+b+)x").unwrap();
    assert!(re.is_match("abx").unwrap());
    assert!(re.is_match("aabbx").unwrap());
    assert!(re.is_match("aaabbbx").unwrap());
    assert!(!re.is_match("ax").unwrap());
    assert!(!re.is_match("bx").unwrap());
    assert!(!re.is_match("abcx").unwrap());
    
    // Test (?<=a(?:b|cd)) positive pattern
    let re2 = Regex::new(r"(?<=a(?:b|cd))x").unwrap();
    assert!(re2.is_match("abx").unwrap());
    assert!(re2.is_match("acdx").unwrap());
    assert!(!re2.is_match("ax").unwrap());
    assert!(!re2.is_match("bcx").unwrap());
}

#[test]
fn test_debug_negative_lookbehind() {
    // First verify that const-size negative lookbehinds work correctly
    let const_re = fancy_regex::Regex::new(r"(?<!a)x").unwrap();
    println!("Const (?<!a)x on 'ax': {:?}", const_re.is_match("ax").unwrap());
    println!("Const (?<!a)x on 'bx': {:?}", const_re.is_match("bx").unwrap());
    
    assert!(!const_re.is_match("ax").unwrap());  // Should NOT match (preceded by a)
    assert!(const_re.is_match("bx").unwrap());   // Should match (not preceded by a)
    
    // Now test a variable-size positive lookbehind to make sure that works
    let pos_var_re = fancy_regex::Regex::new(r"(?<=a+)x").unwrap();
    println!("Positive (?<=a+)x on 'ax': {:?}", pos_var_re.is_match("ax").unwrap());
    println!("Positive (?<=a+)x on 'aax': {:?}", pos_var_re.is_match("aax").unwrap());
    println!("Positive (?<=a+)x on 'bx': {:?}", pos_var_re.is_match("bx").unwrap());
    
    assert!(pos_var_re.is_match("ax").unwrap());   // Should match (preceded by a+)
    assert!(pos_var_re.is_match("aax").unwrap());  // Should match (preceded by a+)
    assert!(!pos_var_re.is_match("bx").unwrap());  // Should NOT match (not preceded by a+)
    
    // Finally test variable-size negative lookbehind
    let neg_var_re = fancy_regex::Regex::new(r"(?<!a+)x").unwrap();
    println!("Negative (?<!a+)x on 'ax': {:?}", neg_var_re.is_match("ax").unwrap());
    println!("Negative (?<!a+)x on 'aax': {:?}", neg_var_re.is_match("aax").unwrap());
    println!("Negative (?<!a+)x on 'bx': {:?}", neg_var_re.is_match("bx").unwrap());
    
    // This should be the opposite of the positive lookbehind
    assert!(!neg_var_re.is_match("ax").unwrap());   // Should NOT match (preceded by a+)
    assert!(!neg_var_re.is_match("aax").unwrap());  // Should NOT match (preceded by a+)
    assert!(neg_var_re.is_match("bx").unwrap());    // Should match (not preceded by a+)
}