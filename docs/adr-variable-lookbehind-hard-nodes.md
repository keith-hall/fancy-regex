# ADR: Variable-Length Lookbehinds with Hard (Backtracking) Nodes

**Status:** Proposed  
**Date:** 2026-08-14

---

## Context

The `compile_lookaround_inner` function in `src/compile.rs` currently restricts
compilation of lookbehind expressions when the inner expression is both *hard*
(requires the backtracking VM — it uses backreferences, atomic groups, look-around,
or conditionals) **and** *variable-size* (the number of bytes matched is not
constant).

The check lives at line ~696:

```rust
let can_compile = inner
    .children
    .iter()
    .all(|child| !child.hard || child.const_size);
```

Any hard child that is also non-`const_size` causes a
`CompileError::FeatureNotYetSupported("Variable length lookbehinds with fancy features")`.

Concrete patterns that are blocked today (with the `variable-lookbehinds` feature
enabled):

1. `(?<=(\w+)\1)x` — backreference inside lookbehind to a variable-length capture
   group; the whole inner expression is hard and variable-size.  Note: a simpler
   pattern like `(?<=(a)\1)x` where the captured group is constant-size (`(a)`)
   would already be handled by the constant-size path — it is the variable-length
   capture group case (e.g. `(\w+)`) that requires the new approach.
2. `(?<=(?i)café)x` — case-insensitive literal inside lookbehind; Unicode
   case-folding can expand/collapse code-point counts, so `LitCasei` for a
   multi-byte string is hard **and** non-`const_size`.
3. `(?<=[a-z](?i)[a-z])x` — `CharClass` with case-insensitive flag inside
   lookbehind; the class can match characters of different UTF-8 byte widths,
   making it non-`const_size`.

---

## Problem Analysis

### Why the current approach works for the easy case

For a **constant-size** lookbehind (hard or easy), the compiler emits a
`GoBack(n)` instruction that rewinds `ix` by exactly `n` bytes before executing
the inner expression forward.  The VM handles this in one step; no backtracking
over the start position is needed because only one start position is valid.

For a **variable-size easy** lookbehind, the compiler emits a
`BackwardsDelegate` instruction.  This instruction runs a *reverse* DFA
(regex-automata with `.reverse(true)`) from `ix` backwards into the haystack to
find the leftmost possible start of the match, advancing `ix` to that start if
found.  This is efficient — O(n) — because the DFA does all the work.

The gap is: **variable-size hard** lookbehinds.  The reverse DFA strategy cannot
be used because `regex-automata` does not support the fancy features (backrefs,
etc.) that make the node "hard" in the first place.

### Why instructions like `LitCasei` and `CharClass` are hard and variable-size

`LitCasei` for a multi-byte string is `hard` because the analysis marks any
literal with `casei: true` as hard when `literal_const_size` returns `false`
(i.e., when Unicode case-folding can produce a different number of bytes than
the source literal — for example, `ß` ↔ `ss`).

`CharClass` for a class containing characters with different UTF-8 byte widths
(e.g., `[a-ÿ]`) is always `hard = false` but `const_size = false`.  If used
inside a lookbehind with `(?i)` the captured class could be hard depending on
flags.

In both cases the VM currently executes these instructions **left-to-right**,
advancing `ix` after a successful match.  Inside a lookbehind this is wrong
because we need to end at the **current** `ix` (the assertion point), not
advance past it.

---

## Decision

We will implement **Approach A — TryGoBack with span-anchored verification**
before exploring other options.

### How TryGoBack works

The key insight is: even though we don't know *how far back* to start, the
*end* of the lookbehind match is always the current position `ix`.  We can
therefore:

1. Save `ix` (the lookbehind end / the assertion point) into a dedicated slot.
2. Try every possible start position from `ix - min_size` down to `0`
   (or `ix - max_size` if a finite max is known), using the existing backtrack
   stack to revisit untried starts on failure.
3. For each candidate start `s`, run the inner expression forward anchored to
   the span `[s, ix]`.  The match must consume exactly the bytes in that span
   — i.e., it must be span-anchored (start at `s` **and** end at `ix`).
4. If any candidate succeeds, the lookbehind succeeds.

#### Early failure and bounding the search

An important optimisation question is: can we abandon a candidate start position
early — before the inner expression finishes — if we can already tell it cannot
produce a match that ends exactly at `ix`?

In general this is hard to guarantee because of backtracking inside the inner
expression (e.g. a repeat could try multiple lengths before giving up).
However, there is one structural bound that is always available: if the inner
expression matches *past* `ix` on some branch, that branch can fail immediately
rather than backtracking further inside the inner expression.  This is already
enforced by the `AssertAtSavedEnd` instruction — it just fires at the end rather
than mid-execution.

A tighter bound can be achieved by tracking a `max_size` on each `Info` node
during analysis (see next section).  With a known `max_size`, the `TryGoBack`
instruction never tries a start position further back than `ix - max_size`,
eliminating candidates that cannot possibly reach `ix` even if the inner
expression matches its maximum extent.

Without `max_size`, the search falls back to scanning all the way to the start of
the string (or start of the match range), which is correct but potentially
wasteful for unbounded patterns like `(\w+)\1`.

#### Span-anchored matching requirement

This is the critical constraint your question highlights.  The inner expression
must match starting at the candidate position **and** end exactly at the
assertion point.  Ordinary forward execution does not enforce the end; the inner
expression would happily keep consuming characters past the assertion point.

Two new instructions handle this:

- **`TryGoBack { min_size: usize, max_size: Option<usize> }`**  
  Emitted just before the inner expression instructions.  When executed:
  - Pushes a backtrack entry `(pc, next_offset)` where `next_offset` is one
    code-point further back (one step more than `ix - min_size`) so that on
    backtrack the next shorter offset is tried.
  - Sets `ix = ix - min_size` (the first, longest candidate start to try).
  - Also saves `ix_end` (the original `ix`) into a temporary slot.

- **`AssertAtSavedEnd { slot: usize }`** (or reusing an existing `Restore`/`Save`
  pair)  
  Emitted immediately **after** the inner expression instructions (just before
  the instruction that resumes the outer pattern).  When executed:
  - Checks that `ix == saved_ix_end`.
  - If not, `break 'fail` — triggers backtracking, which causes `TryGoBack` to
    try the next shorter start position.
  - If yes, continues normally — the lookbehind has matched.

#### Instruction layout emitted by the compiler

```
[TryGoBack { min_size, max_size }]   ← tries ix-min_size, then ix-min_size-1, …
[… inner expression instructions …]
[AssertAtSavedEnd { slot }]          ← enforces that ix ended exactly at the assertion point
[… outer expression continues … ]
```

The existing `GoBack(n)` instruction for constant-size lookbehinds is unchanged.

#### Interaction with the backtrack stack

`TryGoBack` is essentially a `Split`-like instruction but iterating over a
range of offsets rather than two fixed targets.  On each backtrack:

- Pop the state; the saved `next_offset` is one position further back.
- If `next_offset < 0` (or `< ix - max_size` when bounded), the whole candidate
  range is exhausted → `break 'fail` to propagate failure outward.
- Otherwise set `ix = next_offset` and push a new backtrack entry for
  `next_offset - 1`, then continue executing the inner expression.

#### Capture groups inside the lookbehind

Because the inner expression executes forward and the VM manages the save
slots as usual, any capture groups inside the lookbehind are populated
normally — exactly the same as they are today for the constant-size hard case
(which uses `GoBack` then executes forward).

#### Backreferences inside the lookbehind — initial limitation

For a pattern like `(?<=(a)\1)`, when the lookbehind body is entered, group 1
has not yet been captured (it is being captured *inside* the lookbehind for the
first time).  The backref `\1` will work only if the `SaveCaptureGroupStart` /
`Save` for group 1 is set before `\1` is executed.  Because the inner
expression runs left-to-right this is fine: `(a)` sets slot 2/3, then `\1`
reads them.  The starting position for the outer match is the assertion point,
so this should work correctly.  However, if a lookbehind contains a backref to
a group that is *outside* the lookbehind (captured earlier in the main
expression), those slots are set and readable as normal.

#### Performance characteristics

- Worst case: O(n²) per match attempt — for each of the O(n) assertion points
  in the haystack, up to O(n) start offsets are tried.
- In practice, bounded patterns (`{lo,hi}`) keep the inner fan-out small.
- For the patterns named in this ADR (`\1`, `LitCasei`, `CharClass`) the fan-out
  is bounded by the length of the captured group or the literal, which is
  typically small.
- This is acceptable because the VM is already a backtracking engine; adding
  one more bounded iteration is consistent with its existing worst-case
  complexity.

### What needs to change

| File | Change |
|---|---|
| `src/analyze.rs` | Optionally add `max_size: Option<usize>` to `Info` to enable bounded `TryGoBack`; not strictly required (can default to `None` = scan to start of string) |
| `src/vm.rs` | Add `TryGoBack { min_size: usize, max_size: Option<usize> }` and `AssertAtSavedEnd { slot: usize }` to the `Insn` enum; implement both in `run_with` |
| `src/compile.rs` | In `compile_lookaround_inner`, allocate a save slot for `ix_end`, emit `TryGoBack` + inner expression + `AssertAtSavedEnd` for currently-unsupported hard variable-size cases |
| `src/compile.rs` tests | Update unit tests that currently assert `FeatureNotYetSupported` for the now-supported cases |
| `tests/captures.rs` / `tests/finding.rs` | Add integration tests for the three target patterns |

---

## Alternatives Considered

### Approach B: Reverse Execution Mode in the VM

Make the VM capable of executing instructions backwards (right-to-left).  Each
instruction (`Lit`, `LitCasei`, `CharClass`, `Backref`, etc.) would need a
reverse variant that scans leftward from `ix`.

**Rejected for now because:**
- Requires touching almost every instruction handler — large blast radius.
- Reverse `Backref` semantics are non-trivial: the captured text is known but
  matching it backward requires careful off-by-one reasoning.
- Does not obviously perform better than Approach A for small inputs.

### Approach C: Reverse DFA Pre-filter + Forward VM Verification

Decompose the inner expression into an easy part (delegatable to regex-automata
reverse DFA) and a hard part.  Use the reverse DFA to generate candidate start
positions, then run the VM forward on `[candidate, ix]` to verify the hard
constraints.

**Rejected for now because:**
- Requires a two-phase nested execution model (DFA yields candidates, VM
  verifies each).
- Adds significant structural complexity to the instruction set and the VM
  execution loop.
- Approach A is simpler and sufficient for the target patterns.

---

## Consequences

- The three blocked patterns (`(?<=(a)\1)`, `(?<=(?i)café)`, `(?<=[…](?i)[…])`)
  will compile and match correctly under the `variable-lookbehinds` feature.
- Worst-case time complexity increases from O(n) to O(n²) for these patterns,
  which is consistent with the existing backtracking VM for other hard features.
- The `FeatureNotYetSupported` error for hard variable-size lookbehinds is
  removed (or narrowed to truly pathological cases not yet analysed).
- The `BackwardsDelegate` instruction and reverse DFA path are unchanged; they
  continue to handle the easy variable-size case efficiently.
