// Copyright 2024 The Fancy Regex Authors.
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

// Benchmark: Easy patterns (can be delegated to regex-automata)
fn regex_set_easy_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"\d{3}-\d{3}-\d{4}",  // phone number
        r"(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}",  // email (case insensitive)
        r"\d{4}-\d{2}-\d{2}",  // date
        r"https?://[^\s]+",     // URL
    ];
    
    let set = RegexSet::new(&patterns).unwrap();
    let text = "Contact: john@example.com or visit https://example.com";
    
    c.bench_function("regex_set_easy_patterns", |b| {
        b.iter(|| {
            let matches = set.matches(text).unwrap();
            assert!(matches.matched_any());
            matches
        })
    });
}

fn regex_sequential_easy_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"\d{3}-\d{3}-\d{4}",  // phone number
        r"(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}",  // email (case insensitive)
        r"\d{4}-\d{2}-\d{2}",  // date
        r"https?://[^\s]+",     // URL
    ];
    
    let regexes: Vec<_> = patterns.iter().map(|p| Regex::new(p).unwrap()).collect();
    let text = "Contact: john@example.com or visit https://example.com";
    
    c.bench_function("regex_sequential_easy_patterns", |b| {
        b.iter(|| {
            let mut found = None;
            let mut best_pos = usize::MAX;
            
            for (idx, re) in regexes.iter().enumerate() {
                if let Some(m) = re.find(text).unwrap() {
                    if m.start() < best_pos {
                        best_pos = m.start();
                        found = Some(idx);
                    }
                }
            }
            
            assert!(found.is_some());
            found
        })
    });
}

// Benchmark: Hard patterns (require backtracking)
fn regex_set_hard_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"(\w+) \1",           // backreference
        r"\w+(?=!)",           // lookahead
        r"(?<=@)\w+",          // lookbehind
        r"\d+",                // simple pattern
    ];
    
    let set = RegexSet::new(&patterns).unwrap();
    let text = "hello hello world!";
    
    c.bench_function("regex_set_hard_patterns", |b| {
        b.iter(|| {
            let matches = set.matches(text).unwrap();
            assert!(matches.matched_any());
            matches
        })
    });
}

fn regex_sequential_hard_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"(\w+) \1",           // backreference
        r"\w+(?=!)",           // lookahead
        r"(?<=@)\w+",          // lookbehind
        r"\d+",                // simple pattern
    ];
    
    let regexes: Vec<_> = patterns.iter().map(|p| Regex::new(p).unwrap()).collect();
    let text = "hello hello world!";
    
    c.bench_function("regex_sequential_hard_patterns", |b| {
        b.iter(|| {
            let mut found = None;
            let mut best_pos = usize::MAX;
            
            for (idx, re) in regexes.iter().enumerate() {
                if let Some(m) = re.find(text).unwrap() {
                    if m.start() < best_pos {
                        best_pos = m.start();
                        found = Some(idx);
                    }
                }
            }
            
            assert!(found.is_some());
            found
        })
    });
}

// Benchmark: Mixed patterns
fn regex_set_mixed_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"\d+",                // easy
        r"(\w+) \1",           // hard
        r"\w+@\w+\.\w+",       // easy
        r"\w+(?=!)",           // hard
        r"https?://[^\s]+",    // easy
    ];
    
    let set = RegexSet::new(&patterns).unwrap();
    let text = "Check email@example.com or foo foo at https://example.com";
    
    c.bench_function("regex_set_mixed_patterns", |b| {
        b.iter(|| {
            let matches = set.matches(text).unwrap();
            assert!(matches.matched_any());
            matches
        })
    });
}

fn regex_sequential_mixed_patterns(c: &mut Criterion) {
    let patterns = vec![
        r"\d+",                // easy
        r"(\w+) \1",           // hard
        r"\w+@\w+\.\w+",       // easy
        r"\w+(?=!)",           // hard
        r"https?://[^\s]+",    // easy
    ];
    
    let regexes: Vec<_> = patterns.iter().map(|p| Regex::new(p).unwrap()).collect();
    let text = "Check email@example.com or foo foo at https://example.com";
    
    c.bench_function("regex_sequential_mixed_patterns", |b| {
        b.iter(|| {
            let mut found = None;
            let mut best_pos = usize::MAX;
            
            for (idx, re) in regexes.iter().enumerate() {
                if let Some(m) = re.find(text).unwrap() {
                    if m.start() < best_pos {
                        best_pos = m.start();
                        found = Some(idx);
                    }
                }
            }
            
            assert!(found.is_some());
            found
        })
    });
}

// Benchmark: is_match (short circuit on first match)
fn regex_set_is_match(c: &mut Criterion) {
    let patterns = vec![
        r"\d{3}-\d{3}-\d{4}",
        r"(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}",
        r"\d{4}-\d{2}-\d{2}",
    ];
    
    let set = RegexSet::new(&patterns).unwrap();
    let text = "Email: contact@example.com";
    
    c.bench_function("regex_set_is_match", |b| {
        b.iter(|| {
            let result = set.is_match(text).unwrap();
            assert!(result);
            result
        })
    });
}

fn regex_sequential_is_match(c: &mut Criterion) {
    let patterns = vec![
        r"\d{3}-\d{3}-\d{4}",
        r"(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}",
        r"\d{4}-\d{2}-\d{2}",
    ];
    
    let regexes: Vec<_> = patterns.iter().map(|p| Regex::new(p).unwrap()).collect();
    let text = "Email: contact@example.com";
    
    c.bench_function("regex_sequential_is_match", |b| {
        b.iter(|| {
            let mut result = false;
            for re in &regexes {
                if re.is_match(text).unwrap() {
                    result = true;
                    break;
                }
            }
            assert!(result);
            result
        })
    });
}

// Benchmark: Many patterns
fn regex_set_many_patterns(c: &mut Criterion) {
    let patterns: Vec<_> = (0..20).map(|i| format!(r"pattern{}\d+", i)).collect();
    let patterns_ref: Vec<_> = patterns.iter().map(|s| s.as_str()).collect();
    
    let set = RegexSet::new(&patterns_ref).unwrap();
    let text = "Some text pattern10123 more text";
    
    c.bench_function("regex_set_many_patterns", |b| {
        b.iter(|| {
            let matches = set.matches(text).unwrap();
            assert!(matches.matched_any());
            matches
        })
    });
}

fn regex_sequential_many_patterns(c: &mut Criterion) {
    let patterns: Vec<_> = (0..20).map(|i| format!(r"pattern{}\d+", i)).collect();
    let regexes: Vec<_> = patterns.iter().map(|p| Regex::new(p).unwrap()).collect();
    let text = "Some text pattern10123 more text";
    
    c.bench_function("regex_sequential_many_patterns", |b| {
        b.iter(|| {
            let mut found = None;
            let mut best_pos = usize::MAX;
            
            for (idx, re) in regexes.iter().enumerate() {
                if let Some(m) = re.find(text).unwrap() {
                    if m.start() < best_pos {
                        best_pos = m.start();
                        found = Some(idx);
                    }
                }
            }
            
            assert!(found.is_some());
            found
        })
    });
}

criterion_group!(
    name = regex_set_benches;
    config = Criterion::default();
    targets = 
        regex_set_easy_patterns,
        regex_sequential_easy_patterns,
        regex_set_hard_patterns,
        regex_sequential_hard_patterns,
        regex_set_mixed_patterns,
        regex_sequential_mixed_patterns,
        regex_set_is_match,
        regex_sequential_is_match,
        regex_set_many_patterns,
        regex_sequential_many_patterns,
);

criterion_main!(regex_set_benches);
