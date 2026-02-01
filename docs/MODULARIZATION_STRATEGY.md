# Codebase Modularization Strategy

## Overview

This document outlines a strategy for modularizing the fancy-regex codebase to improve maintainability and understandability, particularly for the large source files.

## Current State Analysis

The codebase has several large files:
- `src/parse.rs`: 3,125 lines - Regex parsing into AST
- `src/lib.rs`: 2,179 lines - Public API, core types, and iterators  
- `src/analyze.rs`: 1,201 lines - AST analysis for optimization
- `src/vm.rs`: 1,167 lines - Virtual machine for regex execution
- `src/compile.rs`: 977 lines - AST to VM compilation

While these files are large, they each have a single, clear responsibility. The challenge is that within each file, there are multiple cohesive subfunctions that could be better separated.

## Modularization Principles

When deciding how to split a module, follow these principles:

1. **Maintain API Stability**: The public API should remain unchanged
   - All `pub` items should remain accessible at the same paths
   - Re-export from submodules as needed to preserve import paths
   - Consider using `#[doc(inline)]` to keep documentation structure

2. **Preserve Locality**: Keep related code together (don't over-modularize)
   - Functions that call each other frequently should stay in the same module
   - Data structures and their primary methods should be together
   - Only separate when there's a clear conceptual boundary

3. **Clear Boundaries**: Each module should have a clear, single responsibility
   - Module name should clearly indicate its purpose
   - If you can't succinctly describe what a module does, it may be too broad
   - Avoid "utils" modules except for truly generic helpers

4. **Minimize Cross-Module Dependencies**: Reduce coupling between modules
   - Prefer top-down dependencies (mod.rs -> submodules, not between siblings)
   - Use `pub(super)` or `pub(crate)` to limit visibility
   - If two modules need to call each other, consider if they should be one module

### Examples

**Good**: Splitting parse_atom, parse_group, and parse_class - these handle different regex constructs
**Bad**: Splitting parse_atom into parse_literal, parse_any, parse_assertion - too fine-grained

**Good**: Separating VM instruction definitions from execution engine - clear boundary
**Bad**: Splitting execution.rs into one file per instruction type - too many files

## Proposed Structure

### 1. Parse Module (`src/parse/`)

Convert `src/parse.rs` into a directory structure:

```
src/parse/
├── mod.rs          # Parser struct, parse_re, parse_branch, parse_piece
├── atoms.rs        # parse_atom, parse_escape, parse_hex
├── groups.rs       # parse_group, parse_flags, parse_conditional
├── classes.rs      # parse_class (character classes)
├── backrefs.rs     # Backref and subroutine parsing
└── utils.rs        # parse_decimal, parse_id, make_literal, etc.
```

**Rationale**: Parsing naturally divides by the type of regex construct being parsed. Each module handles a category of similar constructs.

### 2. VM Module (`src/vm/`)

Convert `src/vm.rs` into a directory structure:

```
src/vm/
├── mod.rs          # Insn enum, Prog struct, public interface
├── execution.rs    # run(), State struct, execution loop
└── helpers.rs      # codepoint_len_at, matches_literal, etc.
```

**Rationale**: Separate the instruction definitions from the execution engine from utility helpers.

### 3. Analyze Module (`src/analyze/`)

Convert `src/analyze.rs` into a directory structure:

```
src/analyze/
├── mod.rs          # Public analyze() function, Info struct
└── analyzer.rs     # Analyzer struct and implementation
```

**Rationale**: Separate the public interface from the implementation details of the analyzer.

### 4. Compile Module (`src/compile/`)

Convert `src/compile.rs` into a directory structure:

```
src/compile/
├── mod.rs          # Public compile() function, Compiler struct
├── compiler.rs     # Compiler implementation methods
└── vm_builder.rs   # VMBuilder helper struct
```

**Rationale**: Separate the compiler interface from implementation from the VM builder utility.

### 5. Types Module (`src/types/`)

Extract from `src/lib.rs`:

```
src/types/
├── mod.rs          # Re-exports
├── ast.rs          # Expr, Assertion, LookAround, BacktrackingControlVerb
├── regex.rs        # Regex, RegexBuilder, RegexImpl, RegexOptions
├── match_types.rs  # Match, Captures, Matches, CaptureMatches
└── iterators.rs    # Split, SplitN, SubCaptureMatches, CaptureNames
```

**Rationale**: `lib.rs` contains multiple distinct concerns. Separating types from the API implementation improves clarity.

### 6. Updated `src/lib.rs`

After modularization, `lib.rs` would contain:
- Module declarations
- Re-exports of public types
- Top-level documentation
- Utility functions (escape, prev_codepoint_ix, etc.)

## Implementation Strategy

### General Guidelines

For each file being modularized:

1. **Create Directory and Backup**: Create the subdirectory (e.g., `src/parse/`) and backup the original file
2. **Extract Common Items First**: Move shared types, constants, and imports to `mod.rs`
3. **Create Submodules**: Create each submodule file with appropriate content
4. **Update Visibility**: Change `fn` to `pub(super)` or `pub(crate)` as needed
5. **Add Re-exports**: In `mod.rs`, add `pub use submodule::Type;` for public items
6. **Handle Circular Dependencies**: If modules need to reference each other:
   - Keep them in the same module, OR
   - Move shared types to a separate `types.rs` module, OR  
   - Refactor to eliminate the circular dependency
7. **Update Tests**: Move tests to the appropriate submodule or keep in `mod.rs`
8. **Verify**: Run `cargo check`, `cargo test`, and `cargo fmt`
9. **Delete Original**: Once verified, delete the original `.rs` file

### Phase 1: Parse Module (Highest Value)

The parse module is the largest and most complex. Start here to validate the approach.

**Steps:**
1. Create `src/parse/` directory
2. Move ExprTree and Parser to `parse/mod.rs` 
3. Extract utility functions (parse_decimal, parse_id, make_literal) to `parse/utils.rs`
4. Extract atom parsing methods to `parse/atoms.rs`
5. Extract group parsing methods to `parse/groups.rs`
6. Extract backref parsing methods to `parse/backrefs.rs`
7. Extract class parsing to `parse/classes.rs`
8. Update all internal method calls to use the new module structure
9. Ensure all `pub(crate)` functions are re-exported from `parse/mod.rs`
10. Run tests to verify functionality

**Testing Strategy:**
- Run existing parse module tests after each submodule is created
- Pay special attention to integration tests that use parsing
- Use `cargo check` frequently to catch visibility issues early

### Phase 2: VM Module

The VM is the second-largest file with clear internal structure.

**Steps:**
1. Create `src/vm/` directory
2. Move Insn, Prog, Delegate, CaptureGroupRange to `vm/mod.rs`
3. Extract helper functions to `vm/helpers.rs`
4. Move State struct and run functions to `vm/execution.rs`
5. Update imports and visibility
6. Re-export public items from `vm/mod.rs`
7. Run tests to verify functionality

### Phase 3: Analyze and Compile Modules

These are smaller but would benefit from the same pattern.

**Analyze Steps:**
1. Create `src/analyze/` directory
2. Move public interface (analyze function, Info struct) to `analyze/mod.rs`
3. Move Analyzer implementation to `analyze/analyzer.rs`

**Compile Steps:**
1. Create `src/compile/` directory
2. Move public interface (compile function) to `compile/mod.rs`
3. Move Compiler implementation to `compile/compiler.rs`
4. Move VMBuilder to `compile/vm_builder.rs`

### Phase 4: Types Module (Optional)

This is valuable but more complex due to the public API considerations. Should be done last if at all.

**Considerations:**
- Many types are deeply interconnected (Regex uses Captures, Match, etc.)
- Changing import paths would break existing code
- May be better to leave in `lib.rs` unless the file becomes unwieldy

## Benefits

1. **Easier Navigation**: Developers can quickly find the code they need
2. **Better Understanding**: Each file has a narrow, clear purpose
3. **Reduced Merge Conflicts**: Changes are more likely to touch different files
4. **Improved Testability**: Smaller modules are easier to test in isolation
5. **Clearer Dependencies**: Module boundaries make dependencies explicit

## Trade-offs

1. **More Files**: More files to navigate (mitigated by clear naming)
2. **Initial Churn**: One-time effort to restructure (worth it for long-term maintainability)
3. **Import Complexity**: More `use` statements needed (Rust makes this manageable)

## Success Criteria

After modularization:
- All tests pass
- `cargo check` succeeds with no warnings
- `cargo fmt` produces no changes
- Public API remains unchanged
- Documentation builds correctly
- Code is more maintainable and understandable

## Example: VM Module Refactoring

To demonstrate the approach, here's a concrete example of how `src/vm.rs` (1,167 lines) would be split:

### Before (src/vm.rs)
Single file containing:
- Instruction definitions (Insn enum, Prog struct) ~320 lines
- Execution engine (State struct, run functions) ~600 lines  
- Helper functions (codepoint_len_at, matches_literal, store_capture_groups) ~100 lines
- Tests ~100 lines

### After (src/vm/)

**src/vm/mod.rs** (~350 lines)
```rust
//! VM instruction definitions and program structure

// Re-export public items
pub use execution::{run_default, run_trace};

// Make internal modules available
pub(crate) use execution::run;
pub(crate) use helpers::{codepoint_len_at, matches_literal, store_capture_groups};

mod helpers;
mod execution;

pub enum Insn { /* ... all instruction variants ... */ }
pub struct Prog { /* ... program structure ... */ }
pub struct Delegate { /* ... delegate type ... */ }
pub struct CaptureGroupRange { /* ... range type ... */ }

// Keep instruction and prog implementations here since they're
// closely related to the type definitions
impl Prog { /* ... */ }
impl CaptureGroupRange { /* ... */ }
```

**src/vm/execution.rs** (~600 lines)
```rust
//! VM execution engine and state management

use super::{Insn, Prog, CaptureGroupRange, Delegate};
use super::helpers::*;
use crate::{Error, Result, RegexOptions};

struct State { /* ... execution state ... */ }
struct Branch { /* ... backtracking state ... */ }
struct Save { /* ... saved state ... */ }

impl State { /* ... state methods ... */ }

// These functions are the actual public API from vm.rs
pub fn run_trace(prog: &Prog, s: &str, pos: usize) -> Result<Option<Vec<usize>>> {
    run(prog, s, pos, OPTION_TRACE, &RegexOptions::default())
}

pub fn run_default(prog: &Prog, s: &str, pos: usize) -> Result<Option<Vec<usize>>> {
    run(prog, s, pos, 0, &RegexOptions::default())
}

pub(crate) fn run(
    prog: &Prog,
    s: &str,
    pos: usize,
    options: u32,
    regex_options: &RegexOptions,
) -> Result<Option<Vec<usize>>> {
    /* ... main execution loop ... */
}

#[cfg(test)]
mod tests { /* ... tests for execution ... */ }
```

**src/vm/helpers.rs** (~100 lines)
```rust
//! Helper functions for string matching and capture group management

use regex_automata::util::primitives::NonMaxUsize;
use crate::codepoint_len;
use super::CaptureGroupRange;

/// Returns the length of the UTF-8 codepoint at the given index.
pub(super) fn codepoint_len_at(s: &str, ix: usize) -> usize {
    codepoint_len(s.as_bytes()[ix])
}

/// Checks if a literal string matches at a specific position in the input.
#[inline]
pub(super) fn matches_literal(s: &str, ix: usize, end: usize, literal: &str) -> bool {
    end <= s.len() && &s.as_bytes()[ix..end] == literal.as_bytes()
}

pub(super) fn matches_literal_casei(/*...*/) -> bool { /* ... */ }

/// Helper function to store capture group positions from inner_slots into state.
#[inline]
pub(super) fn store_capture_groups<S: SaveState>(/*...*/) {
    /* ... */
}

pub(super) trait SaveState {
    fn save(&mut self, slot: usize, value: usize);
}
```

**Note**: The function names `run_trace` and `run_default` are actual functions in the current `vm.rs` file (lines 606 and 611 respectively).

### Benefits of This Split

1. **Clear Responsibilities**: Each file has a single, focused purpose
2. **Easier Navigation**: Want to modify execution logic? Go to `execution.rs`. Need to understand instructions? Check `mod.rs`.
3. **Better Encapsulation**: Helper functions are marked `pub(super)`, making it clear they're internal
4. **Improved Testing**: Each module can have its own test module
5. **Reduced Cognitive Load**: Smaller files are easier to understand and modify

## Conclusion

This modularization strategy provides a clear path forward for improving the codebase's maintainability. The approach is conservative, preserving the public API while improving internal organization. Each phase can be implemented independently, allowing for incremental progress and validation.

The key is to balance the benefits of modularization (easier navigation, clearer boundaries) against the costs (more files, potential import complexity). The proposed structure aims for this balance by creating modules at natural conceptual boundaries.
