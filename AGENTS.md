This is a Rust based repository for a regular expression engine.
It has 2 modes, one aims to be compatible with Oniguruma and support the same features.
One aims to be compatible with the Rust regex crate, and extend it to support additional features.
In most cases, these 2 modes overlap cleanly - the regex crate gets extended to support Oniguruma features.

## How it works

The core concept behind this library is to implement a backtracking virtual machine (VM) for regular expression matching, similar to PCRE.
However, whenever possible, this VM delegates work to an underlying regular expression engine - the Rust regex crate - which does not otherwise support "fancy" features like lookarounds and backreferences, but has other desirable design goals - specifically, the regex crate has runtime linear to input length.

For regular expressions that do not use "fancy" features, the library acts primarily as a lightweight wrapper around the underlying engine.
The library performs an analysis to determine which parts of the expression must be handled by the backtracking engine and which can be safely delegated.

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

## Key Guidelines
1. Follow Rust best practices and idiomatic patterns - avoid writing unsafe code.
2. Maintain existing code structure and organization, unless explicitly requested not to.
3. Write unit tests for new functionality.
4. Document public APIs and complex logic. Suggest changes to the Markdown documents when appropriate.
5. Try to re-use existing code where possible - extract common code into helper functions to achieve this.
6. At no point should the `test_utf8.c` file be modified - this is taken from Oniguruma and is how we measure compatibility. If a test fails, we track it in `test_utf8_ignore.c`.
7. When implementing new fancy/hard features, there is no need to modify Expr `to_str` implementation - this is only used to create the `Delegate` instructions for the underlying regex crate, and by definition, the regex crate won't support it.

## Code Standards

### Required Before Each Commit
- Run `cargo fmt` before committing any changes to ensure proper code formatting

### Development Flow
- Test: `cargo test`
- Please run `cargo check` with various combinations of feature flags as well. Check `Cargo.toml` to see the possible features or check how it is done in CI.

## Repository Structure
- `playground/`: Web based playground where fancy-regex is compiled to WASM for easy experimenting
- `src/`: All code
