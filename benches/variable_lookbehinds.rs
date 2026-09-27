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

//! Benchmarks that require the `variable-lookbehinds` feature.
//!
//! Split out of `bench.rs` so the main bench file doesn't need feature gates.
//! When the feature is disabled this file compiles to an empty `main`, so the
//! bench target still exists (it just runs nothing).

#[cfg(feature = "variable-lookbehinds")]
#[macro_use]
extern crate criterion;

#[cfg(feature = "variable-lookbehinds")]
use criterion::Criterion;

#[cfg(feature = "variable-lookbehinds")]
use fancy_regex::internal::{analyze, compile, run_default, AnalyzeContext, CompileOptions};
#[cfg(feature = "variable-lookbehinds")]
use fancy_regex::Expr;
#[cfg(feature = "variable-lookbehinds")]
use fancy_regex::Regex as FancyRegex;

/// The 16-branch JSX tag-start guard from the TypeScript TextMate grammar.
/// The pattern is open, so the lookbehind is attempted at almost every position.
#[cfg(feature = "variable-lookbehinds")]
const LOOKBEHIND_ALT_WIDE: &str = r"(?<!\+\+|--)(?<=[(*,:=>?\[{]|&&|\|\||\?|\*/|^await|[^$._[:alnum:]]await|^return|[^$._[:alnum:]]return|^default|[^$._[:alnum:]]default|^yield|[^$._[:alnum:]]yield|^)\s*<[a-z]+";

#[cfg(feature = "variable-lookbehinds")]
fn const_size_lookbehind(c: &mut Criterion) {
    // Benchmark const-size lookbehind (should use simple GoBack)
    let tree = Expr::parse_tree(r"(?<=ab)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "abx";
    c.bench_function("const_size_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn variable_size_lookbehind(c: &mut Criterion) {
    // Benchmark variable-size lookbehind (uses reverse DFA)
    let tree = Expr::parse_tree(r"(?<=a+b+)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "aaabbbbx";
    c.bench_function("variable_size_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn variable_size_alt_lookbehind(c: &mut Criterion) {
    // Benchmark variable-size lookbehind with alternation
    let tree = Expr::parse_tree(r"(?<=a|bc)x").unwrap();
    let a = analyze(&tree, AnalyzeContext::default()).unwrap();
    let p = compile(
        &a,
        CompileOptions {
            contains_subroutines: tree.contains_subroutines,
            ..CompileOptions::default()
        },
    )
    .unwrap();
    let input = "bcx";
    c.bench_function("variable_size_alt_lookbehind", |b| {
        b.iter(|| run_default(&p, input, 0).unwrap())
    });
}

#[cfg(feature = "variable-lookbehinds")]
fn lookbehind_alternation(c: &mut Criterion) {
    let wide = FancyRegex::new(LOOKBEHIND_ALT_WIDE).unwrap();
    let wide_hay =
        "  return <div>\n    {items.map((x) => <li key={x}>{x}</li>)}\n  </div>;\n".repeat(200);
    c.bench_function("lookbehind_alternation_wide_16", |b| {
        b.iter(|| wide.find_iter(&wide_hay).count())
    });
}

#[cfg(feature = "variable-lookbehinds")]
criterion_group!(
    name = lookbehind_benches;
    config = Criterion::default();
    targets = const_size_lookbehind,
    variable_size_lookbehind,
    variable_size_alt_lookbehind,
    lookbehind_alternation,
);

#[cfg(feature = "variable-lookbehinds")]
criterion_main!(lookbehind_benches);

#[cfg(not(feature = "variable-lookbehinds"))]
fn main() {}
