This is a Rust based repository for a regular expression engine. Please follow these guidelines when contributing:

## How it works

The core concept behind this library is to implement a backtracking virtual machine (VM) for regular expression matching, similar to PCRE.
However, whenever possible, this VM delegates work to an underlying regular expression engine - the Rust regex crate - which does not otherwise support "fancy" features like lookarounds and backreferences, but has other desirable design goals - specifically, the regex crate has runtime linear to input length.

For regular expressions that do not use "fancy" features, the library acts primarily as a lightweight wrapper around the underlying engine.
When such features are present, the library performs an analysis to determine which parts of the expression must be handled by the backtracking engine and which can be safely delegated.

This analysis operates in two phases:

### Phase 1 - Bottom-Up Analysis

Each subexpression is analyzed to determine three key properties:

- *hard*: Whether the subexpression requires backtracking features (backreferences, look-around, atomic groups, conditionals)
- *minimum size*: The minimum number of characters this subexpression will match
- *constant size*: Whether the subexpression always matches the same number of characters

### Phase 2 - Top-Down Compilation

The compilation phase proceeds from the root of the expression, passing a "hard context" that flows from parent to child expressions. This context indicates whether match length variations will affect backtracking decisions.

*Delegation Strategy*: If both the subexpression and context are "easy", the compiler generates a `Delegate` instruction to offload work to the high-performance NFA engine. Otherwise, it generates explicit VM instructions.

*Concatenation Optimization*: For sequences of subexpressions, the compiler employs a sophisticated strategy:

1. Identify a prefix of constant-size, easy subexpressions that can be safely delegated (because they won't affect backtracking)
2. If the context is easy, identify a suffix of easy subexpressions for delegation
3. Compile the remaining "hard" middle section with explicit backtracking instructions
4. The hard context flows from right to left - only the rightmost hard subexpression gets an easy context

This ensures maximum delegation while preserving correct backtracking semantics.

### Summary

In summary, the system efficiently combines backtracking and automaton-based matching by delegating as much work as possible to the underlying high-performance NFA engine, only resorting to backtracking where strictly necessary. This hybrid approach provides both expressive power and performance for advanced regular expression features.

## Code Standards

### Required Before Each Commit
- Run `cargo fmt` before committing any changes to ensure proper code formatting

### Development Flow
- Test: `cargo test`
- Full CI check: `bash ./test.sh`

## Repository Structure
- `playground/`: Web based playground where fancy-regex is compiled to WASM for easy experimenting
- `src/`: All code

## Key Guidelines
1. Follow Rust best practices and idiomatic patterns - avoid writing unsafe code
2. Maintain existing code structure and organization
3. Write unit tests for new functionality
5. Document public APIs and complex logic. Suggest changes to the Markdown documents when appropriate

## Tests

`matching.rs` caters for whether a pattern matches a haystack.
`finding.rs` caters for the position at which a pattern matches a haystack.
`captures.rs` caters for the positions at which capture groups match.

The others are probably self explanatory.
It's also in scope to test that a RuntimeError occurred during matching.

Otherwise for tests that need to check compilation errors, unit tests in analyze.rs or compile.rs (depending which file emits the error) should suffice.

Tests are considered to be unit tests if they are inside `mod test` of a Rust source code file which is in the `src` folder and not in the `tests` folder. One exception could be `lib.rs` because it ties everything together, so here could be integration or unit tests. Prefer writing unit tests where possible, unless it makes sense to have an integration test.

When adding unit tests, try to follow the conventions used in the file, like calling private methods from the `mod test` section instead of more integration style testing building new Regex instances.
