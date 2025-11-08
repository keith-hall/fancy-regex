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
