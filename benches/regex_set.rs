// Copyright 2016 The Fancy Regex Authors.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#[macro_use]
extern crate criterion;

use criterion::Criterion;
use fancy_regex::{Regex, RegexSet};

fn regex_set_simple_patterns(c: &mut Criterion) {
    let patterns = [r"\d{4}-\d{2}-\d{2}", r"\w+@\w+\.\w+", r"\d{3}-\d{3}-\d{4}"];
    let set = RegexSet::new(&patterns).unwrap();
    let input = "Contact user@example.com or call 123-456-7890";

    c.bench_function("regex_set_simple_patterns", |b| {
        b.iter(|| {
            let result = set.matches(input).unwrap();
            assert!(result.pattern().is_some());
        })
    });
}

fn individual_regexes_simple_patterns(c: &mut Criterion) {
    let patterns = [r"\d{4}-\d{2}-\d{2}", r"\w+@\w+\.\w+", r"\d{3}-\d{3}-\d{4}"];
    let regexes: Vec<Regex> = patterns
        .iter()
        .map(|p| Regex::new(p).unwrap())
        .collect();
    let input = "Contact user@example.com or call 123-456-7890";

    c.bench_function("individual_regexes_simple_patterns", |b| {
        b.iter(|| {
            for re in &regexes {
                if re.is_match(input).unwrap() {
                    break;
                }
            }
        })
    });
}

fn regex_set_many_patterns(c: &mut Criterion) {
    let patterns = [
        r"\d{4}-\d{2}-\d{2}",
        r"\w+@\w+\.\w+",
        r"\d{3}-\d{3}-\d{4}",
        r"https?://\S+",
        r"\b\d{5}\b",
        r"#[a-fA-F0-9]{6}",
        r"\b[A-Z]{2,}\b",
        r"\$\d+\.\d{2}",
    ];
    let set = RegexSet::new(&patterns).unwrap();
    // Use input where a later pattern matches
    let input = "Check out style #FF5733 for the design";

    c.bench_function("regex_set_many_patterns", |b| {
        b.iter(|| {
            let result = set.matches(input).unwrap();
            assert!(result.pattern().is_some());
        })
    });
}

fn individual_regexes_many_patterns(c: &mut Criterion) {
    let patterns = [
        r"\d{4}-\d{2}-\d{2}",
        r"\w+@\w+\.\w+",
        r"\d{3}-\d{3}-\d{4}",
        r"https?://\S+",
        r"\b\d{5}\b",
        r"#[a-fA-F0-9]{6}",
        r"\b[A-Z]{2,}\b",
        r"\$\d+\.\d{2}",
    ];
    let regexes: Vec<Regex> = patterns
        .iter()
        .map(|p| Regex::new(p).unwrap())
        .collect();
    // Use input where a later pattern matches
    let input = "Check out style #FF5733 for the design";

    c.bench_function("individual_regexes_many_patterns", |b| {
        b.iter(|| {
            for re in &regexes {
                if re.is_match(input).unwrap() {
                    break;
                }
            }
        })
    });
}

fn regex_set_with_backrefs(c: &mut Criterion) {
    let patterns = [r"(\w+)\s+\1", r"\w+", r"\d+"];
    let set = RegexSet::new(&patterns).unwrap();
    let input = "hello hello world";

    c.bench_function("regex_set_with_backrefs", |b| {
        b.iter(|| {
            let result = set.matches(input).unwrap();
            assert_eq!(result.pattern(), Some(0));
        })
    });
}

fn individual_regexes_with_backrefs(c: &mut Criterion) {
    let patterns = [r"(\w+)\s+\1", r"\w+", r"\d+"];
    let regexes: Vec<Regex> = patterns
        .iter()
        .map(|p| Regex::new(p).unwrap())
        .collect();
    let input = "hello hello world";

    c.bench_function("individual_regexes_with_backrefs", |b| {
        b.iter(|| {
            for re in &regexes {
                if re.is_match(input).unwrap() {
                    break;
                }
            }
        })
    });
}

fn regex_set_long_input(c: &mut Criterion) {
    let patterns = [r"\d{4}-\d{2}-\d{2}", r"\w+@\w+\.\w+", r"https?://\S+"];
    let set = RegexSet::new(&patterns).unwrap();
    let mut input = String::new();
    for _ in 0..100 {
        input.push_str("Some random text without any matches in the middle of the document. ");
    }
    // Add all three potential matches at the end
    input.push_str("Finally we have a date 2024-12-15 and email user@example.com and URL https://example.com here.");

    c.bench_function("regex_set_long_input", |b| {
        b.iter(|| {
            let result = set.matches(&input).unwrap();
            assert!(result.pattern().is_some());
        })
    });
}

fn individual_regexes_long_input(c: &mut Criterion) {
    let patterns = [r"\d{4}-\d{2}-\d{2}", r"\w+@\w+\.\w+", r"https?://\S+"];
    let regexes: Vec<Regex> = patterns
        .iter()
        .map(|p| Regex::new(p).unwrap())
        .collect();
    let mut input = String::new();
    for _ in 0..100 {
        input.push_str("Some random text without any matches in the middle of the document. ");
    }
    // Add all three potential matches at the end
    input.push_str("Finally we have a date 2024-12-15 and email user@example.com and URL https://example.com here.");

    c.bench_function("individual_regexes_long_input", |b| {
        b.iter(|| {
            for re in &regexes {
                if re.is_match(&input).unwrap() {
                    break;
                }
            }
        })
    });
}

criterion_group!(
    name = regex_set_benches;
    config = Criterion::default();
    targets = regex_set_simple_patterns,
    individual_regexes_simple_patterns,
    regex_set_many_patterns,
    individual_regexes_many_patterns,
    regex_set_with_backrefs,
    individual_regexes_with_backrefs,
    regex_set_long_input,
    individual_regexes_long_input,
);

criterion_main!(regex_set_benches);
