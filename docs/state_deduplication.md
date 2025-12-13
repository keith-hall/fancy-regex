# State Deduplication for Catastrophic Backtracking Prevention

This document explains the state deduplication optimization implemented in fancy-regex
to prevent catastrophic backtracking.

## The Problem: Catastrophic Backtracking

Certain regex patterns with nested quantifiers can cause exponential time complexity
when matching against inputs that don't match. For example, the pattern `(a+)+b` with
input `"aaaaaac"` (multiple 'a's followed by 'c' instead of 'b') causes the regex
engine to explore an exponential number of ways to partition the 'a's.

## The Solution: State Deduplication

The VM maintains a HashSet of `(pc, ix)` pairs representing states currently on the
backtrack stack:
- `pc`: program counter (position in the compiled regex program)
- `ix`: input index (position in the input string)

Before pushing a new backtrack state, the VM checks if the same `(pc, ix)` pair is
already on the stack. If it is, the push is skipped, preventing redundant exploration
of the same state.

## Visualizing with the Toy Program

You can use the toy example program to see how the VM compiles patterns that would
exhibit catastrophic backtracking:

### Example 1: Nested Quantifiers with Backreference

```bash
cargo run --example toy graph '(a+)+\1'
```

This generates a DOT graph showing the VM program structure:

```
digraph G {
  0 [label="0: Split(3, 1)"];
  0 -> 3;
  0 -> 1;
  1 [label="1: Any"];
  1 -> 2;
  2 [label="2: Jmp(0)"];
  2 -> 0;
  3 [label="3: Save(0)"];
  3 -> 4;
  4 [label="4: Save(2)"];
  4 -> 5;
  5 [label="5: Lit(\"a\")"];
  5 -> 6;
  6 [label="6: Split(5, 7)"];
  6 -> 5;
  6 -> 7;
  7 [label="7: Save(3)"];
  7 -> 8;
  8 [label="8: Split(4, 9)"];
  8 -> 4;
  8 -> 9;
  9 [label="9: Backref { slot: 2, casei: false }"];
  9 -> 10;
 10 [label="10: Save(1)"];
 10 -> 11;
 11 [label="11: End"];
}
```

The graph shows two nested loops (instructions 5-6 and 4-8) which create the nested
quantifiers. Without state deduplication, matching input like "aaaa...aaac" would
cause the VM to explore exponentially many paths through these loops.

### Example 2: Running with Trace

To see the actual execution with tracing enabled:

```bash
cargo run --example toy trace '(a+)+\1' 'aaaaaac'
```

This shows each instruction executed and backtrack operations. With state
deduplication, you would see that duplicate states are skipped.

## Testing the Optimization

Run the catastrophic backtracking test suite:

```bash
cargo test --test catastrophic_backtracking
```

These tests verify that:
1. Patterns that would cause catastrophic backtracking complete quickly
2. Matches are still found correctly when they exist
3. The optimization doesn't break existing functionality

## Trade-offs

The state deduplication optimization:

**Benefits:**
- Prevents exponential time complexity in many pathological cases
- Reduces likelihood of hitting the backtrack limit
- Minimal performance overhead (HashSet lookup per push)
- Only active when `std` feature is enabled (uses `std::collections::HashSet`)

**Trade-offs:**
- May skip some capture group combinations in patterns with multiple valid matches
- Memory overhead for maintaining the state cache (proportional to stack depth)

In practice, finding one valid match is usually sufficient, so the trade-off is
acceptable for the vast improvement in worst-case performance.

## Implementation Details

The implementation is in `src/vm.rs`:
- `State::state_cache`: HashSet tracking `(pc, ix)` pairs on the stack
- `State::push()`: Checks cache before pushing, adds to cache on successful push
- `State::pop()`: Removes the state from the cache
- `State::backtrack_cut()`: Updates cache when discarding branches

The optimization is only active when the `std` feature is enabled, as it requires
`std::collections::HashSet`.
