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

1. **Maintain API Stability**: The public API should remain unchanged
2. **Preserve Locality**: Keep related code together (don't over-modularize)
3. **Clear Boundaries**: Each module should have a clear, single responsibility
4. **Minimize Cross-Module Dependencies**: Reduce coupling between modules

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

### Phase 1: Parse Module (Highest Value)
The parse module is the largest and most complex. Start here to validate the approach.

1. Create `src/parse/` directory
2. Create submodules with appropriate content
3. Update imports in the new modules
4. Test compilation and all tests
5. Run `cargo fmt`

### Phase 2: VM Module
The VM is the second-largest file with clear internal structure.

### Phase 3: Analyze and Compile Modules
These are smaller but would benefit from the same pattern.

### Phase 4: Types Module (Optional)
This is valuable but more complex due to the public API considerations. Should be done last if at all.

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
