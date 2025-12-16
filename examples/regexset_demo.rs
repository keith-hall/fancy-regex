// Example demonstrating RegexSet usage
use fancy_regex::RegexSet;

fn main() {
    println!("=== RegexSet Examples ===\n");

    // Example 1: Basic usage
    println!("Example 1: Basic usage");
    let set = RegexSet::new(&["foo", "bar", "baz"]).unwrap();
    let text = "hello bar world";
    if let Some(m) = set.find(text).unwrap() {
        println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        println!("  Position: {}..{}", m.start(), m.end());
    }
    println!();

    // Example 2: Priority order (position-based)
    println!("Example 2: Priority order (position-based)");
    let set = RegexSet::new(&["bar", "foo"]).unwrap();
    let text = "foobar";
    if let Some(m) = set.find(text).unwrap() {
        println!("  Text: '{}'", text);
        println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        println!("  'foo' appears first in text, so it wins despite lower priority");
    }
    println!();

    // Example 3: Same position - priority wins
    println!("Example 3: Same position - priority wins");
    let set = RegexSet::new(&["foo", "f", "fo"]).unwrap();
    let text = "foo";
    if let Some(m) = set.find(text).unwrap() {
        println!("  Text: '{}'", text);
        println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        println!("  All patterns match at position 0, highest priority (0) wins");
    }
    println!();

    // Example 4: Mixed easy and hard patterns
    println!("Example 4: Mixed easy and hard patterns");
    let set = RegexSet::new(&[
        r"simple",        // easy pattern
        r"(\w+)\1",       // hard pattern (backreference)
        r"easy",          // easy pattern
    ]).unwrap();
    let text = "foofoo easy simple";
    if let Some(m) = set.find(text).unwrap() {
        println!("  Text: '{}'", text);
        println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        println!("  Hard pattern with backreference matched first");
    }
    println!();

    // Example 5: Lookahead pattern
    println!("Example 5: Lookahead pattern");
    let set = RegexSet::new(&[r"\w+(?=!)", "test"]).unwrap();
    let text = "test! example";
    if let Some(m) = set.find(text).unwrap() {
        println!("  Text: '{}'", text);
        println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        println!("  Lookahead pattern found word before '!'");
    }
    println!();

    // Example 6: No match
    println!("Example 6: No match");
    let set = RegexSet::new(&["foo", "bar"]).unwrap();
    let text = "baz";
    match set.find(text).unwrap() {
        Some(m) => println!("  Matched: {}", m.as_str()),
        None => println!("  No match found in '{}'", text),
    }
    println!();

    // Example 7: Using RegexSetBuilder with max_threads
    #[cfg(feature = "std")]
    {
        println!("Example 7: Using RegexSetBuilder");
        use fancy_regex::RegexSetBuilder;
        let set = RegexSetBuilder::new(&[r"(\w+)\1", r"(\d+)\1"])
            .max_threads(8)
            .build()
            .unwrap();
        let text = "foofoo 123123";
        if let Some(m) = set.find(text).unwrap() {
            println!("  Text: '{}'", text);
            println!("  Pattern {} matched: '{}'", m.pattern(), m.as_str());
        }
        println!();
    }

    println!("=== All examples complete ===");
}
