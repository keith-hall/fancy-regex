# Plan: Unrestricted Capture Group Names (with restricted backrefs)

## Summary

Allow capture group **definitions** and **subroutine calls** to use essentially any characters
in group names (including hyphens, emojis, spaces, punctuation — anything except the closing
delimiter). Backreference syntaxes that support relative recursion levels (`\k<name+N>`,
`\k<name-N>`) remain restricted to `[A-Za-z0-9_\p{L}\p{N}]` to avoid ambiguity.

## Key Architectural Insight

Group names **never** flow to the inner `regex-automata` crate. When expressions are delegated,
`Expr::Group` is serialized as anonymous `(...)` via `Expr::to_str()` (lib.rs:1978). The
`named_groups` mapping (`HashMap<String, usize>` / `BTreeMap<String, usize>`) is maintained
entirely within fancy-regex's own data structures. This means there are **no downstream
constraints** on what characters a group name can contain.

The only validation point is `parse_id()` in `parse.rs`, which uses `is_id_char()` to determine
valid name characters. Everything flows through this single function.

## Current State

```rust
fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
```

`parse_id()` scans characters matching `is_id_char` to build the name, then checks for the
closing delimiter. The `allow_relative: bool` parameter controls whether `+N`/`-N` suffixes
are parsed after the name.

## The Problem with `\k` Backrefs

In `\k<name-3>`, the `-3` is a **relative recursion level** qualifier. This means:
- `\k<foo-1>` = backref to group `foo` at recursion level -1
- If we allowed hyphens in names, `\k<foo-1>` becomes ambiguous: is it group `foo-1` or
  group `foo` at level -1?

The same ambiguity exists with `+` for `\k<name+N>`.

**This ambiguity does NOT exist in other contexts:**
- `(?<foo-1>...)` — unambiguous, close delimiter `>` terminates the name
- `(?P>foo-1)` — unambiguous, close delimiter `)` terminates the name
- `\g<foo-1>` — subroutine calls reject `name+N`/`name-N` combinations anyway (line 371-375),
  and we can refactor to avoid the ambiguity entirely

## Proposed Design: Two Name-Parsing Modes

### Mode 1: "Unrestricted" — for group definitions and unambiguous call sites

Accept **any character** except the closing delimiter. The name is simply "everything between
the open and close delimiters." No `is_id_char` check. Name must be non-empty.

This would be a new function or a new mode in `parse_id`, e.g.:

```rust
/// Parse a name that can contain any characters except the closing delimiter.
/// Returns the name and how many bytes were consumed.
fn parse_unrestricted_name<'a>(
    s: &'a str,
    open: &str,
    close: &str,
) -> Option<(&'a str, usize)> {
    if !s.starts_with(open) {
        return None;
    }
    let after_open = &s[open.len()..];
    let end = after_open.find(close)?;
    if end == 0 {
        return None; // empty name
    }
    let name = &after_open[..end];
    Some((name, open.len() + end + close.len()))
}
```

Or, alternatively, add an `unrestricted: bool` parameter to `parse_id()`. When `true`, the
function skips `is_id_char` checking and just scans to the close delimiter.

### Mode 2: "Restricted" — for backrefs and contexts with relative suffixes

Keep the current `parse_id()` behavior: only `is_id_char` characters, with optional `+N`/`-N`
relative suffixes when `allow_relative` is true.

## Call Sites and Which Mode to Use

| Syntax | Context | Mode | Rationale |
|--------|---------|------|-----------|
| `(?<name>...)` | Group definition | **Unrestricted** | No ambiguity; `>` terminates |
| `(?'name'...)` | Group definition | **Unrestricted** | No ambiguity; `'` terminates |
| `(?P<name>...)` | Group definition | **Unrestricted** | No ambiguity; `>` terminates |
| `\k<name>` | Backref (Oniguruma) | **Restricted** | `-N`/`+N` suffix ambiguity |
| `\k'name'` | Backref (Oniguruma) | **Restricted** | Same ambiguity |
| `(?P=name)` | Backref (Python) | **Restricted** | No relative suffix, but keeping restricted for consistency with `\k`. Users who need unrestricted names can use other backref syntaxes if added, or we revisit later. |
| `\g<name>` | Subroutine call | **Two-pass** | Try unrestricted first (no relative), fall back to restricted+relative for `\g<+1>` etc. |
| `\g'name'` | Subroutine call | **Two-pass** | Same |
| `(?P>name)` | Subroutine call | **Unrestricted** | No ambiguity; `)` terminates |
| `(?(<name>)...)` | Conditional | **Restricted** | Uses `allow_relative: true` |
| `(?('name')...)` | Conditional | **Restricted** | Same |
| `${name}` | Template expansion | **Restricted** | References groups; should use safe chars |
| `$name` (undelimited) | Template expansion | **Restricted** | Any special char would be ambiguous with literal text |
| `\g<name>` (Python expand) | Template expansion | **Restricted** | Same |

## Implementation Steps

### Step 1: Add `parse_unrestricted_name()` function

```rust
/// Parse a group name that allows any characters except the closing delimiter.
/// The name must be non-empty. Returns `(name, total_bytes_consumed)`.
pub(crate) fn parse_unrestricted_name<'a>(
    s: &'a str,
    open: &str,
    close: &str,
) -> Option<(&'a str, usize)> {
    if !s.starts_with(open) || s.len() <= open.len() + close.len() {
        return None;
    }
    let after_open = &s[open.len()..];
    // Find the close delimiter
    let end = after_open.find(close)?;
    if end == 0 {
        return None; // empty name not allowed
    }
    let name = &after_open[..end];
    Some((name, open.len() + end + close.len()))
}
```

This function is simple and self-contained — no `is_id_char`, no relative suffix handling.

### Step 2: Update capture group definition parsing (parse_group)

For `(?<name>...)`, `(?'name'...)`, and `(?P<name>...)`, replace `parse_id()` calls with
`parse_unrestricted_name()`:

```rust
// Before (line 865-873):
if let Some(ParsedId { id, relative: None, skip }) =
    parse_id(&self.re[ix + 1..], open, close, false)
{
    group_name = Some(id.to_string());
    (None, skip + 1)
}

// After:
if let Some((name, skip)) =
    parse_unrestricted_name(&self.re[ix + 1..], open, close)
{
    group_name = Some(name.to_string());
    (None, skip + 1)
}
```

Same change for the `(?P<name>...)` path (line 880-888).

### Step 3: Update `(?P>name)` subroutine call parsing

```rust
// Before (line 901-902):
} else if self.re[ix..].starts_with("?P>") {
    return self.parse_delimited_subroutine_call(ix + 3, "", ")", false);

// After:
} else if self.re[ix..].starts_with("?P>") {
    return self.parse_unrestricted_subroutine_call(ix + 3, "", ")");
```

Add a new helper or inline the unrestricted parsing:

```rust
fn parse_unrestricted_subroutine_call(
    &mut self, ix: usize, open: &str, close: &str
) -> Result<(usize, Expr)> {
    if let Some((name, skip)) = parse_unrestricted_name(&self.re[ix..], open, close) {
        let target = if let Ok(num) = name.parse::<usize>() {
            self.numeric_capture_group_references = true;
            if num == 0 { self.self_recursive = true; }
            CaptureGroupTarget::ByNumber(num)
        } else {
            CaptureGroupTarget::ByName(name.to_string())
        };
        self.contains_subroutines = true;
        Ok((ix + skip, Expr::AstNode(AstNode::SubroutineCall(target), ix)))
    } else {
        Err(Error::ParseError(ix, ParseError::InvalidGroupName))
    }
}
```

### Step 4: Refactor `\g<name>` / `\g'name'` subroutine calls (two-pass)

The `\g<...>` syntax needs to handle both:
- Unrestricted names: `\g<foo-bar>`, `\g<🎯>`, `\g<hello world>`
- Pure relative: `\g<+1>`, `\g<-2>`

Two-pass approach in `parse_delimited_subroutine_call`:

```rust
fn parse_delimited_subroutine_call(
    &mut self, ix: usize, open: &str, close: &str, allow_relative: bool,
) -> Result<(usize, Expr)> {
    // First: try unrestricted name (no relative suffix)
    if let Some((name, skip)) = parse_unrestricted_name(&self.re[ix..], open, close) {
        // Check it's not a pure +N/-N (those should go through the relative path)
        let trimmed = name.trim();
        let is_pure_relative = !trimmed.is_empty()
            && (trimmed.starts_with('+') || trimmed.starts_with('-'))
            && trimmed[1..].parse::<usize>().is_ok();

        if !is_pure_relative {
            let target = if let Ok(num) = name.parse::<usize>() {
                self.numeric_capture_group_references = true;
                if num == 0 { self.self_recursive = true; }
                CaptureGroupTarget::ByNumber(num)
            } else {
                CaptureGroupTarget::ByName(name.to_string())
            };
            self.contains_subroutines = true;
            return Ok((ix + skip, Expr::AstNode(AstNode::SubroutineCall(target), ix)));
        }
    }

    // Second: try as relative reference (+N / -N)
    if allow_relative {
        if let Some(ParsedId { id, relative: Some(rel), skip }) =
            parse_id(&self.re[ix..], open, close, true)
        {
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

### Step 5: Leave backref parsing UNCHANGED

`parse_delimited_backref` continues to use `parse_id()` with the existing `is_id_char`
restriction and `allow_relative` support. No changes needed.

### Step 6: Leave expand.rs UNCHANGED

Template expansion continues to use `parse_id()` with restricted names. Users reference
groups in templates using the restricted character set. This is a reasonable limitation since
template strings have their own escaping concerns.

### Step 7: Leave conditional group parsing UNCHANGED

`(?(<name>)...)` and `(?('name')...)` continue to use `parse_id()` with restricted names.

## Edge Cases

### What names are valid?

Under unrestricted parsing, ALL of these would be valid group names:
- `(?<foo-bar>...)` — hyphens ✅
- `(?<my group>...)` — spaces ✅
- `(?<🎯>...)` — emojis ✅
- `(?<hello, world!>...)` — punctuation ✅
- `(?<data-value-1>...)` — CSS-like names ✅
- `(?<café>...)` — accented chars ✅ (already works)

The only restriction: the name cannot contain the closing delimiter character (`>` for
`(?<...>...)`, `'` for `(?'...'...)`).

### Names that could be confusing

- `(?<123>...)` — purely numeric name. `id.parse::<usize>()` succeeds, so it would be
  treated as a numeric group override? No — in group definitions, the name is always stored
  as a name string. The numeric parse only matters for backrefs/subroutine call resolution.
  Actually, looking at the code, group definitions always go through `group_name = Some(name.to_string())`
  so this is fine — it's stored as a named group.

- `(?<+1>...)` — looks like a relative reference but it's a group definition. Group
  definitions don't support relative refs, so this would just be a group named `+1`. This
  seems fine.

- `(?<>...)` — empty name. Rejected by `end == 0` check. ✅

### Referencing unrestricted names

If a user defines `(?<foo-bar>...)`, they can reference it via:
- `\g<foo-bar>` — ✅ (two-pass subroutine call)
- `(?P>foo-bar)` — ✅ (unrestricted subroutine call)
- `\k<foo-bar>` — ❌ (restricted backref — ambiguous with recursion level)
- `(?P=foo-bar)` — ❌ (restricted backref — kept for consistency)
- `${foo-bar}` — ❌ (restricted template expansion)

Users who define groups with exotic names and need backrefs would be limited. This is an
intentional trade-off: the `\k<name-N>` recursion level syntax makes unrestricted backref
names unsafe. A future extension could add a new backref syntax for unrestricted names.

### What about `(?P=name)` backrefs?

`(?P=name)` uses `allow_relative: false`, so there's no `+N`/`-N` ambiguity. We *could*
safely allow unrestricted names there. This would let users define `(?<foo-bar>.)` and
reference it with `(?P=foo-bar)`. 

**Recommendation:** Consider allowing unrestricted names in `(?P=name)` in a follow-up change.
For the initial implementation, keep it restricted for simplicity and consistency.

## Backward Compatibility

- **No existing patterns break.** Previously-invalid names (containing hyphens, spaces, etc.)
  become valid. No existing valid pattern changes meaning.
- The `is_id_char` function is unchanged; it continues to be used for restricted contexts.
- The `parse_id` function is unchanged; a new `parse_unrestricted_name` function is added
  alongside it.

## Files Changed

| File | Changes |
|------|---------|
| `src/parse.rs` | Add `parse_unrestricted_name()`. Update group definition parsing to use it. Update `(?P>name)` to use it. Refactor `parse_delimited_subroutine_call` for two-pass. |
| `src/parse.rs` (tests) | Add tests for unrestricted names in group defs, subroutine calls. Confirm backrefs remain restricted. Update existing negative tests. |
| `tests/captures.rs` | Integration tests for capturing with unrestricted group names. |
| `tests/matching.rs` | Integration tests for subroutine calls with unrestricted names. |

## Test Plan

### Positive tests — group definitions
- `(?<foo-bar>a)` — hyphenated name
- `(?<data-value-1>a)` — CSS-style name
- `(?'foo-bar'a)` — single-quote syntax with hyphen
- `(?P<foo-bar>a)` — Python syntax with hyphen
- `(?<🎯>a)` — emoji name
- `(?<hello world>a)` — space in name
- `(?<a->a)` — trailing hyphen
- `(?<-a>a)` — leading hyphen
- `(?<a b c>a)` — multiple spaces

### Positive tests — subroutine calls
- `(?<foo-bar>a)\g<foo-bar>` — `\g` with hyphenated name
- `(?<foo-bar>a)\g'foo-bar'` — `\g` single-quote with hyphen
- `(?<foo-bar>a)(?P>foo-bar)` — Python subroutine with hyphen
- `\g<foo-bar>(?<foo-bar>a)` — forward reference
- `(?<🎯>a)\g<🎯>` — emoji subroutine call
- `(?<🎯>a)(?P>🎯)` — emoji Python subroutine call

### Positive tests — relative references still work
- `(a)\g<+1>(b)` — relative forward subroutine
- `(a)(b)\g<-1>` — relative backward subroutine
- `(a)\k<-1>` — relative backward backref
- `(a)\k<+1>(b)` — relative forward backref
- `(?<n>a)\k<n-0>` — named backref with recursion level

### Negative tests — backrefs remain restricted
- `\k<foo-bar>` — fails (ambiguous with recursion level)
- `\k'foo-bar'` — fails
- `(?P=foo-bar)` — fails (restricted for consistency)

### Negative tests — empty names still rejected
- `(?<>a)` — fails
- `(?P<>a)` — fails
- `(?''a)` — fails

### Edge cases
- `(?<>>a)` — name cannot contain `>` (group unclosed parse error)
- `(?<a'b>a)` — `'` is fine in angle-bracket delimited name
- `(?'a>b'a)` — `>` is fine in single-quote delimited name
- `(?<123>a)` — numeric-looking name, stored as named group
