# Reverse search feasibility: direction-on-`RegexInput` vs `rev-prog`

## Scope

This note evaluates whether reverse search should be implemented by:

1. Adding a direction flag to `RegexInput` and running VM programs “backwards” in `run`, versus
2. The current `rev-prog` branch approach (new reverse APIs + reverse candidate scanning + anchored forward checks).

It also evaluates whether this can help variable-length lookbehind without relying on regex-automata reverse delegates.

## What `rev-prog` currently does

From `origin/rev-prog`:

- Adds public reverse APIs:
  - `find_previous`, `find_previous_input`, `find_previous_from_pos`
  - `captures_previous`, `captures_previous_input`, `captures_previous_from_pos`
- Adds `RegexInput::to_pos(end)` for reverse-prefix constraints.
- Adds reverse VM entry points (`run_rev*`) that:
  - iterate candidate start positions from right to left,
  - run the existing forward VM anchored at each candidate.
- Adds `BackwardsProg` so hard variable-length lookbehind can run a nested anchored subprogram in reverse-search mode.
- For Wrap/delegated regex, reverse search is also implemented as right-to-left candidate scanning with anchored forward checks.

So `rev-prog` avoids a full reverse VM; it reuses the forward VM for each anchored candidate.

## Feasibility of “single VM run starting at last instruction”

### Short answer

Not feasible as a small/incremental change. It is possible in principle, but would be a major VM/compiler redesign with high regression risk.

### Why it is hard

Current bytecode and runtime are fundamentally forward-oriented:

- Control flow is compiled for forward execution (`pc=0`, `End` as terminal, `Split/Jmp` targets assume forward graph).
- Many opcodes are direction-sensitive:
  - consuming ops (`Any`, `Lit`, `CharClass`),
  - search/scan ops (`SplitUnanchored`, `Seek`, `AbsentRepeater`),
  - delegate ops (`Delegate`, `BackwardsDelegate`),
  - capture semantics (`SaveCaptureGroupStart`) assume forward monotonic position.
- Assertions are expressed at a position boundary, but “start/end” style assertions and scan bootstrap logic depend on forward attempt flow.
- Capture slot usage is ordered around forward entry/exit (`start` then `end`), including overwrite rules tied to position ordering.

Because of this, “start at last instruction and walk backward” is not just an interpreter toggle. It needs either:

- a mirrored instruction set with reversed control-flow edges, or
- a compiler pass producing dedicated reverse bytecode with reverse-safe capture logic.

Both are invasive.

## Pros/cons comparison

## `rev-prog` approach (as-is)

### Pros

- Lower implementation risk: reuses existing forward VM semantics.
- Easier to validate correctness (same anchored forward matching core).
- Enables hard variable-length lookbehind now (`BackwardsProg`) without a full reverse bytecode engine.
- Keeps lookbehind implementation local (nested reverse search over prefix).

### Cons

- Public API expansion is larger than ideal.
- Reverse matching can be expensive (right-to-left candidate scan + anchored retry per candidate).
- Duplicates reverse wrappers for both Wrap and Fancy paths.

## Direction-on-`RegexInput` + full reverse VM execution

### Pros

- Potentially cleaner external API surface if direction is part of input configuration.
- Conceptually unifies forward/reverse query shape.

### Cons

- Very invasive engine/compiler changes.
- High bug risk across captures, backtracking, assertions, seek/bootstrap, and delegates.
- Significant design/testing cost before parity is trustworthy.

## Recommendation

Do **not** pursue “execute existing program backward from the last instruction” as the next step.

A better path is:

1. Keep `rev-prog`’s core strategy (reverse candidate scan + anchored forward match).
2. Reduce API invasiveness by introducing direction on `RegexInput` and routing existing `find_input`/`captures_input` through it.
3. Keep explicit convenience methods only if needed for ergonomics.
4. Continue using localized reverse mode for variable lookbehind subprograms (`BackwardsProg`/equivalent), not a global VM-direction flip.

This preserves feasibility and correctness while still moving toward a cleaner API model.

## Proposed implementation plan

### Phase 1: API shape consolidation

- Add `SearchDirection` (`Forward`, `Reverse`) to `RegexInput`.
- Add builder method on `RegexInput` to set direction.
- Route `find_input` and `captures_input` by input direction internally.
- Implement previous-match behavior through reverse direction, optionally retaining `find_previous*`/`captures_previous*` as thin wrappers (or deprecate later).

### Phase 2: Runtime integration

- Keep forward VM unchanged.
- Keep reverse runtime strategy as candidate scanning with anchored forward checks.
- Factor shared reverse-scan helper(s) so Wrap/Fancy paths share logic.
- Ensure range/start/anchored semantics remain consistent in both directions.

### Phase 3: Variable-lookbehind alignment

- Continue compiling hard variable-length lookbehind to a nested subprogram reverse check.
- Pass reverse intent locally for subprogram execution (without mutating caller-visible input state).
- Verify capture propagation rules from nested reverse checks (especially overlapping and optional groups).

### Phase 4: validation + performance guardrails

- Expand tests for:
  - zero-width reverse matches,
  - range-bounded reverse queries,
  - wrap/fancy parity,
  - variable-lookbehind captures.
- Add micro-benchmarks for reverse-heavy workloads and document complexity trade-offs.
- If needed, add optional prefilters/skip heuristics later; keep correctness-first baseline.

## Decision criteria before full merge

Proceed only if all are satisfied:

- No regression in existing forward APIs/behavior.
- Reverse semantics are deterministic and documented (especially zero-width tie-breaking).
- Variable-lookbehind correctness holds for captures and backrefs in supported feature sets.
- Performance is acceptable for target use cases (or limitations are documented).
