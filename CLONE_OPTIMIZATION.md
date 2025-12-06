# Removing Clone Trait from Insn and Prog

## Problem Statement

The `Insn` (instruction) struct in the VM implementation had a `Clone` trait, and it was unclear why it was needed. This investigation aimed to determine if the Clone trait was unnecessary and if removing it could improve performance.

## Investigation

### Where Clone was Used

1. **Insn enum**: Defined with `#[derive(Clone, Debug)]` in `src/vm.rs`
2. **Prog struct**: Also derived Clone, containing `Vec<Insn>` as the body
3. **RegexImpl::Fancy variant**: Contained a `Prog` directly, which was cloned when a `Regex` was cloned

### How Instructions are Used

The VM execution function `vm::run()` takes a `&Prog` reference, meaning it never needs to own or clone the program. When matching against instructions in the main execution loop, the code uses `match prog.body[pc]` which matches by reference, with individual variants using `ref` patterns where needed (e.g., `Insn::Lit(ref val)`).

### Cloning Behavior

When a `Regex` was cloned (e.g., for thread safety in multithreaded applications), it would:
1. Clone the `RegexImpl` enum
2. If the variant was `Fancy`, clone the entire `Prog`
3. Clone the entire `Vec<Insn>`, which meant cloning every single instruction

## Solution

Instead of cloning `Prog`, we wrapped it in an `Arc<Prog>` in the `RegexImpl::Fancy` variant. This allows multiple `Regex` instances to share the same program without cloning all instructions.

### Changes Made

1. Changed `RegexImpl::Fancy` to use `Arc<Prog>` instead of `Prog`
2. Wrapped `Prog` in `Arc::new()` when constructing the `Fancy` variant
3. Removed `Clone` derive from both `Insn` enum and `Prog` struct

## Performance Impact

### Benchmark Results

We measured the performance of cloning a `Regex` with a complex pattern:

**Before optimization:**
- Time per clone: ~2.8µs (2,813ns)

**After optimization:**
- Time per clone: ~74ns

**Improvement: ~38x faster cloning!**

### Existing Benchmarks

Running the existing benchmark suite showed:
- No regression in regex execution performance
- Slight improvement (4-5%) in some backtracking tests
- All tests pass without issues

## Conclusion

The Clone trait on `Insn` and `Prog` was only needed because `Regex` stored `Prog` directly. By using `Arc<Prog>` instead, we:
1. Made `Regex` cloning dramatically faster (38x improvement)
2. Reduced memory usage when multiple `Regex` instances exist
3. Successfully removed the unnecessary Clone trait from VM instructions
4. Maintained all existing functionality and performance characteristics

This optimization is particularly beneficial in scenarios where:
- Regex instances are cloned for thread safety
- Multiple threads need to use the same compiled regex
- Applications cache and reuse regex patterns

## Code Locations

- `src/lib.rs`: Changed `RegexImpl::Fancy` to use `Arc<Prog>`
- `src/vm.rs`: Removed `Clone` derive from `Insn` and `Prog`
