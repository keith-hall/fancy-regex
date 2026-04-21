# Plan: Seek Instruction

## Overview

Currently, the fancy-regex VM searches for a match by attempting the full backtracking program at every position in the haystack (via the `SplitUnanchored` preamble). This is O(n) VM invocations in the length of the haystack, which is fine for short strings but can be very expensive for long haystacks with patterns like `(..)\1\1\1$`.

This plan describes how to implement a **Seek** pre-filter: a compiled inner `regex-automata` pattern that can quickly find the earliest position in the haystack where the VM *might* match, so the VM only needs to be run from that position onward instead of every position.

---

## Background: How the VM Works Today

1. `lib.rs` / `Regex::new_options` parses → analyzes → compiles the pattern into a `Prog`.
2. For non-anchored patterns, `compile.rs` prepends a `SplitUnanchored` / `Any` / `Jmp` preamble that bumps `ix` by one character each time the pattern fails at the current position.
3. The VM in `vm.rs` then runs the full program starting from the user-supplied `pos`.

The bottleneck for patterns like `(..)\1\1\1$` on a 2000-character haystack is that the VM tries 2000 starting positions, each of which requires backtracking work.

---

## Concept: The Seek Pattern

A **seek pattern** is a regular (non-backtracking) approximation of the original fancy pattern. It is derived by replacing every "hard" (backtracking-requiring) node with its *simplest regular equivalent*:

| Hard node | Seek approximation |
|---|---|
| `\1` (backref to group 1 whose content is `(..)`) | Replace with the inlined group body: `..` |
| `\g<1>` (subroutine call to group 1) | Inline the group body the same way (up to recursion depth 20, matching existing `MAX_SUBROUTINE_RECURSION_DEPTH`) |
| `(?=...)` / `(?!...)` / `(?<=...)` / `(?<!...)` | Drop entirely (zero-width, can't appear in a positional seek); positive trailing lookaheads are already tree-shaken |
| `(?>...)` (atomic group) | Keep the contents as-is (atomicity is invisible to the seek approximation) |
| `\K` | Drop |
| `(*FAIL)` and other backtracking verbs | Drop |
| Conditional `(?(cond)...)` | Use the union of the true and false branches |
| Absent operator | Drop |

The key invariant is: **the seek pattern must never skip a true match position** (it may report false-positive positions). In other words, it is a conservative over-approximation.

Example:

```
Original:  (..)\1\1\1$
Seek:      (..)......$   →  simplified further to  .{8}$
```

(The capture group can be removed when generating the seek pattern because we only need to find the position, not record spans.)

---

## Implementation Plan

### Phase 0: Benchmarks First

Before any code changes, add a benchmark covering the motivating example so we can measure the impact.

1. In `benches/bench.rs`, add a new benchmark function `seek_backref_in_long_haystack` that:
   - Compiles `(..)\1\1\1$`
   - Runs `find` on a haystack that is a single line of 2000 random characters (which has exactly one match at the end)
   - The benchmark exercises the worst-case "must scan the whole haystack" scenario

2. Add a paired benchmark `seek_backref_in_long_haystack_no_match` where the haystack contains no match, to measure the false-positive cost.

These benchmarks will serve as the before/after comparison point.

### Phase 1: Seek Pattern Generation (in `compile.rs`)

Add a new function (or method on `Compiler`) `build_seek_pattern` that walks an `Info` tree and produces an `Option<String>` — the seek pattern as a string, or `None` if no useful seek pattern can be constructed.

**Algorithm:**

```
fn seek_pattern(info: &Info, group_info_map: &Map<usize, &Info>, depth: usize) -> Option<String>
```

Walk the `Expr` tree recursively, building a `regex_syntax` / plain string representation:

- `Expr::Literal`, `Expr::Any`, `Expr::Delegate`, `Expr::Repeat`, `Expr::Alt`, `Expr::Concat`, `Expr::Assertion` (anchors like `^`/`$`/`\b`) — emit these unchanged (they are already regular).
- `Expr::Group` — emit the content without the capture group wrapper (we only need position, not spans).
- `Expr::Backref { group }` — look up `group` in `group_info_map`, inline its seek pattern (like a subroutine call), respecting `depth < MAX_SUBROUTINE_RECURSION_DEPTH`.
- `Expr::SubroutineCall(target)` — inline the seek pattern of the target group, respecting the depth limit.
- `Expr::LookAround(_, LookAhead)` — for positive lookaheads, the tree-shaker already moves them outside capture group 0; drop in seek context. For others, drop entirely.
- `Expr::AtomicGroup` — keep contents, drop the atomicity wrapper.
- `Expr::KeepOut` / `Expr::ContinueFromPreviousMatchEnd` / `Expr::BacktrackingControlVerb` / `Expr::BackrefExistsCondition` / `Expr::Conditional` / `Expr::Absent` — drop (emit `""` or `"(?:)"`).
- `Expr::GeneralNewline` — emit `\R` or `(?:\r\n|[\n\x0B\x0C\r\x85\u{2028}\u{2029}])`.

Return `None` if the resulting pattern string is empty (i.e. the entire pattern degenerates to the empty pattern — no useful pruning is possible) or if building the inner `regex-automata` regex fails (e.g. exceeds size limits).

**Heuristic for usefulness:** Only emit a seek pattern if:
- The pattern contains at least one hard node (otherwise the VM path is not used, so there is nothing to optimise), **and**
- The resulting seek approximation is not just `.*` / `(?s:.*)` (i.e. it contains at least one anchor, literal, or constrained character class).

For the MVP, a simple check suffices: the seek pattern string must contain at least one non-`.` non-`*` non-`+` non-`?` non-`{` non-`(` non-`)` non-`|` character that is not a quantifier — i.e. it contains a literal, a character class `[...]`, or an anchor `^`/`$`/`\b`.

### Phase 2: New `Insn::Seek` VM Instruction

Add a new instruction variant to `vm.rs`:

```rust
/// Seek to the next position in the haystack where `inner` could match.
/// Replaces the `SplitUnanchored` / `Any` / `Jmp` preamble when a useful seek pattern exists.
Seek {
    /// The compiled seek pre-filter regex.
    inner: Regex,
    /// The seek pattern string (for debug display).
    pattern: String,
}
```

**VM execution semantics for `Seek`:**

```
Insn::Seek { inner, .. } =>
    // Find the next match of `inner` starting at `ix` in `s`.
    // Use `search_half` with an un-anchored search across `s[ix..]`.
    match inner.find_at(s, ix):
        None  => return Ok(None)          // no position could ever match; fail definitively
        Some(m) =>
            if m.start() == ix:
                // already at a valid position; proceed to the next instruction
                pc += 1; continue
            else:
                // advance ix to the start of the seek match
                // but first push a backtrack point so we can try the next position if this
                // position fails.
                push(pc, m.start() + 1)   // next seek attempt starts one past current seek match
                ix = m.start()
                pc += 1; continue
```

*False-positive handling:* when the VM fails at `ix = m.start()`, it pops to `(pc_seek, m.start() + 1)` and the `Seek` instruction is re-executed from one byte past the previous seek-match start, so it finds the next candidate position.

*Empty-match handling:* The `Seek` instruction must not loop infinitely on zero-length seek matches. Before pushing the backtrack point, if `m.start() == m.end()`, the "next" seek offset is `m.end() + 1` (or one `codepoint_len` forward).

### Phase 3: Emitting `Seek` During Compilation

In `compile.rs`, modify the `compile` function. After the analysis confirms the pattern is hard *and* a seek pattern was successfully generated:

**Option A — Replace the `SplitUnanchored` preamble:**

Currently the non-anchored preamble is:
```
SplitUnanchored(entry, bump)
Any
Jmp(0)
entry: <program body>
```

Replace with:
```
Seek { inner, pattern }
<program body>
```

The `Seek` instruction takes over the role of `SplitUnanchored`: it advances `ix` to the next candidate position instead of bumping one character at a time.

When `Seek` is not applicable (anchored patterns, or no useful seek pattern), the existing `SplitUnanchored` preamble is emitted unchanged.

**Integration with `OPTION_FIND_NOT_EMPTY`:** If `find_not_empty` is set, the `Seek` instruction should record `match_attempt_start = ix` before proceeding (the same way `SplitUnanchored` does), so that zero-length match rejection still works.

**Integration with `OPTION_SKIPPED_EMPTY_MATCH`:** The `Seek` instruction receives this flag in `option_flags`; when set and `ix == pos`, it should advance one code-point before seeking, mirroring the behaviour of `SplitUnanchored`.

### Phase 4: Builder API

Add an opt-in flag to `RegexOptions` and expose it via `RegexOptionsBuilder` / `RegexBuilder`:

```rust
/// Enable the Seek pre-filter optimisation for hard (backtracking) patterns.
///
/// When enabled, the compiler attempts to derive a regular approximation of the pattern
/// which is used to skip to the earliest plausible match position in the haystack before
/// invoking the backtracking VM. This can dramatically speed up searches in long haystacks
/// when the pattern can only match at infrequent positions.
///
/// The seek pattern is always a conservative over-approximation — it may report false-positive
/// positions but will never skip a true match.
///
/// Default is `false`.
pub fn seek(&mut self, yes: bool) -> &mut Self
```

Store the flag in `RegexOptions` and thread it through to `CompileOptions` / the compiler.

For the MVP, the flag defaults to `false` so existing users are not affected. The plan is to flip the default to `true` once benchmarks confirm there is no regression on common patterns.

### Phase 5: Seek State in the VM Run Loop

The `Seek` instruction's backtrack point needs to know the correct next offset to try. The approach in Phase 2 (pushing `m.start() + 1` as the next `ix` for the `Seek` instruction) means the backtrack stack entry is `(pc_seek, ix_next)`. When the VM pops that entry:

- `pc = pc_seek` (points to the `Seek` instruction again)
- `ix = ix_next` (starts the next seek from one past the previous candidate)

The `Seek` instruction then searches for the next match of `inner` starting at the new `ix`. This is clean and reuses the existing backtracking machinery.

### Phase 6: Interaction with `find_iter` / `captures_iter`

`Matches::next_with` calls `find_from_pos_with_option_flags` with `pos = last_end`. Because the `Seek` instruction re-runs from `pos` on each invocation of the VM (the VM is reset between calls), the first seek in each call starts at `pos`, exactly like the current `SplitUnanchored` preamble. No changes are needed in `lib.rs` for the iterator case.

### Phase 7: Future — RegexSet Integration

When a `RegexSet` API is added, the seek patterns for each member regex can be collected and fed to `regex-automata`'s `meta::Regex::new_many` (or a `RegexSetBuilder`), allowing a single multi-pattern DFA scan to identify which regexes have candidate positions and where. The VM would then be invoked only for those regexes at those positions. To enable this:

- Expose the compiled `Seek` regex (or the seek pattern string) from `Prog` as a public field.
- Add a `skip_seek: bool` flag to the VM's runtime options (alongside `backtrack_limit`) so the RegexSet runner can tell individual VM invocations not to re-do the seek that already happened externally.

This is out of scope for the MVP but the architecture in Phases 2–6 is designed to accommodate it without major refactoring.

---

## File-by-File Summary of Changes

| File | Changes |
|---|---|
| `benches/bench.rs` | New benchmarks for seek on long haystack (match and no-match) |
| `src/vm.rs` | New `Insn::Seek { inner: Regex, pattern: String }` variant; VM execution logic for it |
| `src/compile.rs` | New `build_seek_pattern(info, group_info_map) -> Option<String>` function; modify `compile()` to emit `Insn::Seek` instead of the `SplitUnanchored` preamble when applicable |
| `src/lib.rs` | Add `seek: bool` to `RegexOptions`; expose via `RegexOptionsBuilder::seek()` and `RegexBuilder::seek()` |
| `tests/` | New integration tests in `matching.rs` / `finding.rs` verifying seek gives identical results to the non-seek VM for a range of patterns with backrefs, subroutine calls, anchors |

---

## Testing Strategy

### Correctness

Add tests in `tests/matching.rs` and `tests/finding.rs` that compare `seek(true)` against `seek(false)` for the following pattern classes:

- Backreference patterns: `(..)\1\1\1$`, `(\w+) \1`, `(.)(.)\2\1`
- Subroutine call patterns: `(a\g<1>?b)`, `(\w+)\g<1>`
- Anchored patterns: `^foo\1` — seek should not change behaviour (anchored, so `SplitUnanchored` is not emitted)
- Patterns with no useful seek: `(a*)b\1` — seek pattern degenerates, falls back to `SplitUnanchored`
- Patterns with lookaheads: `(\w+)(?=!)` — the tree-shaker already handles these; ensure seek doesn't break them
- Empty haystack, single-char haystack, exact-length haystack (edge cases for the offset arithmetic)

### Performance

Run `cargo bench` before and after to confirm that:
- `seek_backref_in_long_haystack` is significantly faster with the seek flag
- Existing benchmarks show no regression

---

## Open Questions / Future Work

1. **Multiple seek sub-patterns:** As noted in the problem statement, some patterns (e.g. `\d+(?=\w).{3}foo`) could benefit from two seek sub-expressions (`\d+\w` and `\d+.{3}foo`) where the VM only proceeds when all agree. The MVP uses a single seek pattern; this can be revisited once the basic infrastructure is in place.

2. **Default on/off:** Start with `seek(false)` as the default to avoid any surprise performance changes. After benchmarking on real-world patterns, consider flipping to `true` by default (or making it conditional on a heuristic like "pattern length > N" or "contains at least one anchor or literal").

3. **Integration with `SplitUnanchored`:** The problem statement mentions possibly merging `Seek` into `SplitUnanchored`. The current plan keeps them separate for clarity and easier rollback. If profiling shows the dispatch cost matters, they could be merged later.

4. **`no_std` compatibility:** `regex-automata`'s `meta::Regex` is available in `no_std` contexts (the `std` feature is not required). The `Seek` instruction should compile in both `std` and `no_std` builds.

5. **Seek for easy patterns:** Easy patterns are currently handled by the `RegexImpl::Wrap` path which delegates entirely to `regex-automata`; that engine already does internal pre-filtering (literal search, etc.), so no seek instruction is needed for them.
