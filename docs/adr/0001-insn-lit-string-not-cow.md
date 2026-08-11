# ADR 0001: Keep `Insn::Lit` as `String`, not `Cow` or `Arc`

## Status

Accepted

## Context

`vm.rs` contained the comment `// should be cow?` on the `Insn::Lit(String)` variant,
suggesting that the inner `String` could be replaced with `Cow<'_, str>` (or a similar
reference-counted / borrowed type) to avoid an allocation.

The question is: would `Cow<'_, str>` or `Arc<str>` improve performance here?

### How `Insn::Lit` is produced

During regex compilation the expression tree (`Expr::Literal { val: String, casei }`) is
walked once and each non-case-insensitive literal node produces:

```rust
self.b.add(Insn::Lit(val.clone()));
```

This clones the string from the `Expr` tree into the compiled `Prog`.

### Lifetime of each value

| Value | Lives in | Dropped when |
|---|---|---|
| `Expr::Literal { val, .. }` | temporary analysis tree | compilation finishes |
| `Insn::Lit(String)` | `Prog`, behind `Arc<Prog>` | `Regex` is dropped |

The `Expr` tree is **not** stored in the final `Regex` struct — only `Arc<Prog>` is kept.
This means:

* **`Cow<'a, str>` (borrowed variant)** — there is nothing long-lived to borrow from.
  The `Expr` nodes are gone by the time matching begins, so the `Cow` would have to be
  in the `Owned` state at runtime, providing no benefit over `String`.

* **`Arc<str>`** — would share the allocation between `Expr::Literal.val` and
  `Insn::Lit` during the brief compilation window only.  Once compilation finishes the
  `Expr` side is dropped.  At steady state (during matching) the memory footprint is
  identical to the current `String`.  The only gain would be avoiding one string copy
  per literal *at compile time*, but regex compilation is a one-time cost and literal
  strings are typically short.  The added indirection (pointer-chasing through the
  `Arc` allocation) could even be a minor regression on hot matching paths.

  Changing `Expr::Literal.val` to `Arc<str>` would also be a **breaking public API
  change** with no meaningful runtime benefit.

## Decision

Keep `Insn::Lit(String)`. Remove the stale `// should be cow?` comment.

## Consequences

* No API change.
* No runtime performance change.
* The codebase is slightly clearer without the misleading comment.
