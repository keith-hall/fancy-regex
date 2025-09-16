use fancy_regex::Regex;

#[test]
fn test_variable_lookbehind_patterns() {
    // These should currently fail with LookBehindNotConst
    let result1 = Regex::new(r"(?<=a+b+)");
    println!("Pattern (?<=a+b+): {:?}", result1);
    assert!(result1.is_err());
    
    let result2 = Regex::new(r"(?<=a(?:b|cd))");
    println!("Pattern (?<=a(?:b|cd)): {:?}", result2);
    assert!(result2.is_err()); 
    
    // This should work (alternation with const sizes)
    let result3 = Regex::new(r"(?<=a|bc)");
    println!("Pattern (?<=a|bc): {:?}", result3);
    assert!(result3.is_ok());
}