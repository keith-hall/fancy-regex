# Plan: Allow Hyphens in Capture Group Names and Named Subroutine Calls

## Background

The codebase already has a TODO at `parse.rs:1490` acknowledging this need:
```rust
// TODO: if !allow_relative, also eat - char... Oniguruma example: (?<foo-+a>a)\g<foo-+a>\k<foo-+a>
```

The core challenge is that `-` is overloaded:
- In backrefs like `\k<name-3>`, the `-3` means "relative recursion level"
- In relative refs like `\k<-1>`, the `-1` means "relative group number"
- But Oniguruma also allows hyphens *in names themselves* in capture group definitions and subroutine calls

## Architecture of the Change

Everything flows through one function: **`parse_id()`** (line 1476). It takes an `allow_relative: bool` parameter that controls whether `+N`/`-N` suffixes are parsed. The key insight from the TODO is:

> **When `allow_relative` is `false`, hyphens can safely be part of the name because there's no ambiguity with relative offsets.**

Here's every call site and what should change:

| Syntax | Context | `allow_relative` | Should allow `-` in name? | Reason |
|--------|---------|-------------------|---------------------------|--------|
| `(?<name>...)` | Group definition | `false` | ✅ **Yes** | No ambiguity — close delimiter is `>` |
| `(?'name'...)` | Group definition | `false` | ✅ **Yes** | No ambiguity — close delimiter is `'` |
| `(?P<name>...)` | Group definition | `false` | ✅ **Yes** | No ambiguity — close delimiter is `>` |
| `\k<name>` | Backref | `true` | ❌ **No** | `-N` suffix means recursion level; `\k<foo-bar>` is ambiguous with `\k<foo-bar(number)>` |
| `\k'name'` | Backref | `true` | ❌ **No** | Same ambiguity |
| `(?P=name)` | Backref (Python) | `false` | ❌ **No** | Although no relative suffix, allowing hyphens here would be inconsistent with `\k` and confusing |
| `\g<name>` | Subroutine call | `true` | ❌ **No** | Although `name+N` is rejected for subroutine calls, the *parsing* still uses `allow_relative=true` to detect and reject it. A hyphen in the name would be ambiguous at the parse level. |
| `\g'name'` | Subroutine call | `true` | ❌ **No** | Same as above |
| `(?P>name)` | Subroutine call | `false` | ✅ **Yes** | No ambiguity — close delimiter is `)`, no relative suffixes |
| `(?(<name>)...)` | Conditional | `true` | ❌ **No** | Same relative ambiguity |
| `(?('name')...)` | Conditional | `true` | ❌ **No** | Same |
| `${name}` | Template expansion | `false` | ❓ **Maybe** | Could allow it, but likely should match backref behavior (disallow) since expansions reference groups by name |
| `$name` (undelimited) | Template expansion | `false` | ❌ **No** | `-` would be ambiguous with subtraction/literal text |

## Proposed Implementation

### Step 1: Add a new parameter to `parse_id()` (or derive behavior from existing params)

The cleanest approach: add an `allow_hyphen: bool` parameter to `parse_id()`. When `true`, `is_id_char` also accepts `-`.

**Alternative (from the TODO):** Derive `allow_hyphen` from `!allow_relative`. This is simpler but slightly less flexible — it would automatically allow hyphens in `(?P=name)` and `(?P>name)` contexts. Looking at the call sites:

- `(?P=name)` calls with `allow_relative: false` — we'd get hyphens in backrefs, which we said we don't want
- `(?P>name)` calls with `allow_relative: false` — we *do* want hyphens here

So the TODO's suggestion of tying it to `!allow_relative` doesn't perfectly match our requirements. A separate `allow_hyphen: bool` parameter is better.

### Step 2: Update `parse_id()` signature and logic

```rust
pub(crate) fn parse_id<'a>(
    s: &'a str,
    open: &'_ str,
    close: &'_ str,
    allow_relative: bool,
    allow_hyphen: bool,      // NEW
) -> Option<ParsedId<'a>> {
```

Change line 1490 from:
```rust
let after_id = iter.find(|(_, ch)| !is_id_char(*ch));
```
to:
```rust
let after_id = iter.find(|(_, ch)| !is_id_char(*ch) && !(allow_hyphen && *ch == '-'));
```

There's also a `debug_assert` at line 1482 (`debug_assert!(!close.starts_with(is_id_char))`) — this should remain valid since `-` is not an `is_id_char`, and close delimiters (`>`, `'`, `)`) don't start with `-`.

### Step 3: Refactor `parse_delimited_subroutine_call`

The `parse_delimited_subroutine_call` function currently passes `allow_relative: true` to `parse_id`, but it then **rejects** any result where `relative.is_some()` and `id` is non-empty (line 371-375). The `allow_relative: true` is only needed for purely relative calls like `\g<+1>` / `\g<-1>`.

Refactor to two-pass approach:

```rust
fn parse_delimited_subroutine_call(&mut self, ix: usize, open: &str, close: &str, allow_relative: bool) -> Result<(usize, Expr)> {
    // First try: parse as a full name (possibly containing hyphens)
    if let Some(ParsedId { id, relative: None, skip }) = parse_id(&self.re[ix..], open, close, false, true) {
        let target = if let Ok(num) = id.parse::<usize>() {
            self.numeric_capture_group_references = true;
            if num == 0 { self.self_recursive = true; }
            CaptureGroupTarget::ByNumber(num)
        } else {
            CaptureGroupTarget::ByName(id.to_string())
        };
        self.contains_subroutines = true;
        return Ok((ix + skip, Expr::AstNode(AstNode::SubroutineCall(target), ix)));
    }
    // Second try: parse as pure relative reference (+N / -N)
    if allow_relative {
        if let Some(ParsedId { id, relative: Some(rel), skip }) = parse_id(&self.re[ix..], open, close, true, false) {
            if id.is_empty() {
                let target = CaptureGroupTarget::Relative(rel);
                self.contains_subroutines = true;
                return Ok((ix + skip, Expr::AstNode(AstNode::SubroutineCall(target), ix)));
            }
        }
    }
    Err(Error::ParseError(ix, ParseError::InvalidGroupName))
}
```

### Step 4: Backrefs stay restrictive

For `\k<name>`, the situation is harder because `\k<name-3>` is **valid** (recursion level qualifier). `\k<foo-bar>` is genuinely ambiguous if `bar` is a number. `parse_delimited_backref` continues using `allow_hyphen: false`.

`(?P=name)` is called with `allow_relative: false`, so there's no `-N` ambiguity. We **could** safely allow hyphens there. But then we have asymmetry: `(?P=foo-bar)` works but `\k<foo-bar>` doesn't. This seems acceptable — it's the same kind of asymmetry Oniguruma has.

**Decision:** Allow hyphens in `(?P=name)` backrefs since it's unambiguous.

### Step 5: Update all call sites

| Call site | `allow_hyphen` |
|-----------|---------------|
| `(?<name>...)` group def (line 869) | `true` |
| `(?'name'...)` group def (line 869) | `true` |
| `(?P<name>...)` group def (line 884) | `true` |
| `\k<name>` backref (line 326) | `false` |
| `\k'name'` backref (line 326) | `false` |
| `(?P=name)` backref (line 894) | `true` (unambiguous) |
| `\g<name>` subroutine (two-pass) | `true` then `false` |
| `\g'name'` subroutine (two-pass) | `true` then `false` |
| `(?P>name)` subroutine (line 902) | `true` |
| `(?(<name>)...)` conditional (line 1048) | `false` |
| `(?('name')...)` conditional (line 1046) | `false` |
| Template expansion (expand.rs) | `false` |

## Edge Cases

1. **Name starting/ending with hyphen:** `(?<-foo>...)` or `(?<foo->...)` — should be valid. Simplest implementation treats `-` as a valid name char when `allow_hyphen` is true.

2. **Name that is only hyphens:** `(?<-->...)` — valid syntactically. Unusual but harmless.

3. **Name that looks numeric with hyphens:** `(?<1-2>...)` — `id.parse::<usize>()` fails, so treated as a named group. Correct.

4. **Backward compatibility:** Existing valid patterns are unaffected since hyphens were previously rejected.

5. **`debug_assert` on line 1482:** Since `-` is not in `is_id_char`, and close delimiters don't start with `-`, the assert remains valid. Add: `debug_assert!(!(allow_hyphen && close.starts_with('-')))`.

6. **`parse_usize` on the "relative" path:** When `allow_hyphen` is true and we encounter `-`, we eat it as part of the name, so the relative-offset path is never reached. This is desired.

## Test Plan

1. **Positive tests — group definitions:**
   - `(?<foo-bar>a)` matches and captures
   - `(?'foo-bar'a)` matches and captures
   - `(?P<foo-bar>a)` matches and captures

2. **Positive tests — subroutine calls with hyphenated names:**
   - `(?<foo-bar>a)\g<foo-bar>` — two-pass approach
   - `(?<foo-bar>a)(?P>foo-bar)` — Python syntax
   - `\g<foo-bar>(?<foo-bar>a)` — forward reference

3. **Positive tests — backrefs with hyphenated names (Python syntax):**
   - `(?<foo-bar>.)(?P=foo-bar)` — ✅ should work

4. **Negative tests — backrefs with hyphenated names (Oniguruma syntax):**
   - `\k<foo-bar>` — still fails (ambiguous with recursion level)
   - `\k'foo-bar'` — still fails

5. **Negative tests — relative references still work:**
   - `(a)\k<-1>` — still works
   - `(a)\g<+1>(b)` — still works
   - `(a)\k<1-0>` — still works

6. **Edge case tests:**
   - `(?<->a)` — name is just `-`
   - `(?<a-b-c>a)` — multiple hyphens
