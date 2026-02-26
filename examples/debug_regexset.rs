use fancy_regex::RegexSet;

fn main() {
    // Test the exact failing case
    let text = "ab1c2";
    let pattern = r"\d*(?=[a-z])";
    let set = RegexSet::new(&[pattern]).unwrap();
    for m in set.matches(text) {
        let m = m.unwrap();
        println!("{:?} pattern={}", m.range(), m.pattern());
    }
}
