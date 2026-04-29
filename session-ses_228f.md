# Cargo dead code warnings in tests

**Session ID:** ses_228f19120ffeWqvZcBTDww36gw
**Created:** 4/29/2026, 5:25:40 AM
**Updated:** 4/29/2026, 5:26:24 AM

---

## User

running `cargo check --tests` warns me about some dead code, but that code is used, just by tests. Do we need to put some cfg annotation on the function so it wouldn't give us this false warning?

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 5.1s)

_Thinking:_

The user is asking about dead code warnings in Rust when running `cargo check --tests`. They want to know if they need to add `#[cfg(test)]` or some other attribute to suppress false dead code warnings for functions that are only used by tests.

Let me first look at the codebase to understand the current state, then I can provide accurate advice.

**Tool: bash**

**Input:**
```json
{
  "command": "cargo check --tests 2>&1 | grep -E \"warning|dead_code\" | head -50",
  "workdir": "/Users/keith.hall/repos/fancy-regex"
}
```

**Output:**
```
warning: function `regex` is never used
  = note: `#[warn(dead_code)]` on by default
warning: `fancy-regex` (test "regex_options") generated 1 warning

```

_Thinking:_



---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 2.9s)

**Tool: bash**

**Input:**
```json
{
  "command": "cargo check --tests 2>&1 | grep -B 5 \"never used\"",
  "workdir": "/Users/keith.hall/repos/fancy-regex"
}
```

**Output:**
```
warning: function `regex` is never used

```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 3.9s)

**Tool: bash**

**Input:**
```json
{
  "command": "cargo check --tests 2>&1",
  "workdir": "/Users/keith.hall/repos/fancy-regex"
}
```

**Output:**
```
warning: function `regex` is never used
 --> tests/common/mod.rs:4:8
  |
4 | pub fn regex(re: &str) -> Regex {
  |        ^^^^^
  |
  = note: `#[warn(dead_code)]` on by default

warning: `fancy-regex` (test "regex_options") generated 1 warning
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.07s

```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 2.8s)

**Tool: read**

**Input:**
```json
{
  "filePath": "/Users/keith.hall/repos/fancy-regex/tests/common/mod.rs"
}
```

**Output:**
```
<path>/Users/keith.hall/repos/fancy-regex/tests/common/mod.rs</path>
<type>file</type>
<content>
1: use fancy_regex::Regex;
2: 
3: #[cfg(test)]
4: pub fn regex(re: &str) -> Regex {
5:     let parse_result = Regex::new(re);
6:     assert!(
7:         parse_result.is_ok(),
8:         "Expected regex '{}' to be compiled successfully, got {:?}",
9:         re,
10:         parse_result.err()
11:     );
12:     parse_result.unwrap()
13: }
14: 
15: use std::fmt;
16: pub struct DebugRegex<'a>(pub &'a Regex);
17: impl fmt::Display for DebugRegex<'_> {
18:     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
19:         self.0.debug_print(f)
20:     }
21: }

(End of file - total 21 lines)
</content>
```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 5.0s)

_Thinking:_

The issue is in `tests/common/mod.rs`. The `regex` function is marked with `#[cfg(test)]`, but this is in a test file (`tests/` directory) that is only compiled for integration tests. Since it's already in the `tests/` folder, the `#[cfg(test)]` annotation is redundant and actually doesn't help here.

The warning says that `regex` is never used in the `regex_options` test binary. Let me check which test files are using this `regex` function.

**Tool: bash**

**Input:**
```json
{
  "command": "ls tests/",
  "workdir": "/Users/keith.hall/repos/fancy-regex"
}
```

**Output:**
```
captures.rs
common
finding.rs
matching.rs
oniguruma
oniguruma.rs
regex_options.rs
replace.rs
send_sync.rs
splitting.rs

```

_Thinking:_



---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 3.4s)

**Tool: bash**

**Input:**
```json
{
  "command": "grep -l \"common::regex\\|use common\" /Users/keith.hall/repos/fancy-regex/tests/*.rs"
}
```

**Output:**
```
/Users/keith.hall/repos/fancy-regex/tests/captures.rs
/Users/keith.hall/repos/fancy-regex/tests/finding.rs
/Users/keith.hall/repos/fancy-regex/tests/matching.rs
/Users/keith.hall/repos/fancy-regex/tests/replace.rs

```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 2.6s)

**Tool: bash**

**Input:**
```json
{
  "command": "head -5 /Users/keith.hall/repos/fancy-regex/tests/regex_options.rs"
}
```

**Output:**
```
use fancy_regex::{CompileError, Error, Regex, RegexBuilder};
//use fancy_regex::common::DebugRegex;
mod common;

fn build_regex(builder: &RegexBuilder) -> Regex {

```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 7.8s)

The issue is clear. `tests/regex_options.rs` includes `mod common;` but never calls `common::regex()`, so when Rust compiles the `regex_options` test binary, it sees `regex` as unused.

The fix is **not** `#[cfg(test)]` — that attribute is already there but is redundant in a `tests/` file and doesn't help. The correct annotation is `#[allow(dead_code)]` on the function itself, since it's a shared helper that happens not to be used by every test binary that includes the module.

**Tool: edit**

**Input:**
```json
{
  "filePath": "/Users/keith.hall/repos/fancy-regex/tests/common/mod.rs",
  "oldString": "#[cfg(test)]\npub fn regex(re: &str) -> Regex {",
  "newString": "#[allow(dead_code)]\npub fn regex(re: &str) -> Regex {"
}
```

**Output:**
```
Edit applied successfully.
```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 4.4s)

**Tool: bash**

**Input:**
```json
{
  "command": "cargo check --tests 2>&1",
  "workdir": "/Users/keith.hall/repos/fancy-regex"
}
```

**Output:**
```
    Checking fancy-regex v0.18.0 (/Users/keith.hall/repos/fancy-regex)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 1.01s

```

---

## Assistant (Code · Anthropic: Claude Sonnet 4.6 · 6.6s)

No more warnings. Here's the explanation:

**Why `#[cfg(test)]` was wrong here:** Code inside `tests/` is already only compiled during testing — `#[cfg(test)]` is for conditionally compiling test helpers inside `src/`. Using it in `tests/common/mod.rs` had no effect.

**Why the warning occurred:** Each file in `tests/` is compiled as a separate test binary. `regex_options.rs` does `mod common;` but never calls `common::regex()`, so the Rust compiler (correctly, from that binary's perspective) warns it's unused.

**Why `#[allow(dead_code)]` is the right fix:** The function is a shared helper in a common module. Not every test binary needs every helper, so suppressing the lint on the function itself is the appropriate solution — it signals intentional shared-but-not-universally-used code.

---

