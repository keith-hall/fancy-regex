use fancy_regex::Regex;
use std::time::Instant;

fn main() {
    // Test 1: Nested quantifiers
    println!("Testing nested quantifiers (a+)+b with non-matching input...");
    let re = Regex::new(r"(a+)+b").unwrap();
    
    for n in [10, 15, 20, 25].iter() {
        let input = "a".repeat(*n) + "c";
        let start = Instant::now();
        let result = re.is_match(&input);
        let duration = start.elapsed();
        
        println!("  n={}: {:?} - result: {:?}", n, duration, result.unwrap());
    }
    
    // Test 2: Backreference pattern
    println!("\nTesting backreference pattern (a+)+\\1 with non-matching input...");
    let re = Regex::new(r"^(a+)+\1$").unwrap();
    
    for n in [10, 12, 14, 15].iter() {
        let input = "a".repeat(*n) + "b";
        let start = Instant::now();
        let result = re.is_match(&input);
        let duration = start.elapsed();
        
        println!("  n={}: {:?} - result: {:?}", n, duration, result.unwrap());
    }
}
