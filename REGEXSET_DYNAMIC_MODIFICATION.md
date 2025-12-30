# RegexSet Dynamic Modification Specification

## Problem Statement

Add the ability to add or remove a `Regex` from a `RegexSet` even between iterations of a haystack match. For example:

```rust
let mut set = RegexSet::new(&[...])?;
let mut iter = set.matches(haystack);
let first = iter.next();
set.remove_at_index(6);  // Remove pattern at index 6
let second = iter.next(); // Should not match pattern 6 anymore
```

If there is an easy way to make this possible without a high performance cost of cloning etc., implement it directly. Otherwise, document possible solutions in this specification.

**Note:** If it makes the implementation easier, we could restrict modifications to only adding and removing regexes at the end of the list.

## Current Architecture

The current `RegexSet` implementation has the following structure:

```rust
pub struct RegexSet {
    inner: Arc<RegexSetImpl>,
}

struct RegexSetImpl {
    easy_patterns: Option<EasyPatternSet>,
    patterns: Vec<Pattern>,
}

pub struct RegexSetMatches<'h> {
    set: &'h RegexSet,  // Immutable borrow
    haystack: &'h str,
    range: Range<usize>,
    current_pos: usize,
    pattern_cache: Vec<Option<(usize, usize, Captures<'h>)>>,
    easy_next_match: Option<(usize, Range<usize>)>,
    pattern_set: Option<PatternSet>,
}
```

### Key Constraints

1. **Immutable Borrowing**: The iterator holds an immutable reference to `RegexSet`, preventing any modifications while iterating
2. **Arc Wrapping**: `RegexSetImpl` is wrapped in an `Arc`, making it inherently immutable and shared
3. **Multi-Pattern DFA**: Easy patterns are compiled into a single `regex_automata::meta::Regex` (DFA), which cannot be modified dynamically
4. **Iterator State**: The iterator maintains cached state for each pattern that would need to be invalidated/updated on modification

## Possible Solutions

### Solution 1: Interior Mutability with RwLock (Moderate Performance Cost)

Replace `Arc<RegexSetImpl>` with `Arc<RwLock<RegexSetImpl>>`:

```rust
pub struct RegexSet {
    inner: Arc<RwLock<RegexSetImpl>>,
}
```

**Pros:**
- Allows modification while iterators exist
- Thread-safe
- Relatively straightforward to implement

**Cons:**
- Every iterator access requires acquiring a read lock (performance overhead)
- Write operations block all readers
- Each `next()` call would need multiple lock acquisitions
- Multi-pattern DFA would need to be rebuilt on each modification (expensive)

**Performance Impact:** Moderate to High
- Read lock acquisition on every iterator operation
- Full DFA rebuild on modification (could be hundreds of microseconds to milliseconds for many patterns)

### Solution 2: Copy-on-Write with Arc::make_mut (Low Performance Cost for Reads)

Keep the current `Arc` structure but use `Arc::make_mut` for modifications:

```rust
impl RegexSet {
    pub fn add_regex(&mut self, regex: Regex) -> Result<()> {
        let inner = Arc::make_mut(&mut self.inner);
        // Add pattern and rebuild DFA if necessary
    }
    
    pub fn remove_at_index(&mut self, index: usize) -> Result<()> {
        let inner = Arc::make_mut(&mut self.inner);
        // Remove pattern and rebuild DFA if necessary
    }
}
```

**Pros:**
- Zero cost for read operations (no locking)
- Only clones when there are multiple references
- Iterators remain unaffected (they keep their snapshot)
- Clean API requiring `&mut self`

**Cons:**
- Requires `&mut self`, preventing modification while iterating (by Rust's ownership rules)
- Iterators would continue using the old pattern set (may or may not be desired)
- Not possible to modify during iteration as requested

**Performance Impact:** Low (but doesn't meet requirement)
- No overhead for existing iterators
- Modification creates a new version if there are other references

### Solution 3: Version-Based Snapshot with Epoch Counter (Low Cost, Complex)

Use a generation/epoch counter to detect when the set has been modified:

```rust
pub struct RegexSet {
    inner: Arc<RwLock<RegexSetImpl>>,
    generation: Arc<AtomicU64>,
}

pub struct RegexSetMatches<'h> {
    set: Arc<RwLock<RegexSetImpl>>,
    generation_snapshot: u64,
    generation: Arc<AtomicU64>,
    // ... other fields
}
```

On each `next()` call, check if `generation` has changed and invalidate cache if needed.

**Pros:**
- Can detect modifications
- Iterators can decide how to handle modifications (error, continue, etc.)
- Relatively low overhead for read-heavy workloads

**Cons:**
- Complex implementation
- Still requires locking
- Cache invalidation adds complexity
- Pattern indices might become invalid after removal

**Performance Impact:** Moderate
- Lock acquisition on each operation
- Generation check is cheap (atomic load)

### Solution 4: Restrict to Append/Remove at End Only (Simplest)

Simplify the problem by only allowing adding or removing patterns at the end:

```rust
impl RegexSet {
    pub fn push_regex(&mut self, regex: Regex) -> Result<()> {
        // Add pattern at the end
        // Update easy_patterns DFA if it's an easy pattern
    }
    
    pub fn pop_regex(&mut self) -> Option<Regex> {
        // Remove pattern from the end
        // Rebuild easy_patterns DFA if necessary
    }
}
```

**Pros:**
- Pattern indices remain stable
- Simpler to implement
- Can still use `Arc::make_mut` approach
- Iterator cache entries for removed patterns can be safely ignored

**Cons:**
- Limited functionality (can only modify the end)
- Still requires `&mut self`, preventing modification during iteration
- DFA rebuild still required for easy pattern changes

**Performance Impact:** Low (but still can't modify during iteration)

### Solution 5: Iterator-Local Masking (Low Cost, Meets Requirements)

Instead of modifying the `RegexSet`, allow iterators to have a "mask" of enabled patterns:

```rust
pub struct RegexSetMatches<'h> {
    set: &'h RegexSet,
    enabled_patterns: BitSet,  // Which patterns are currently enabled
    // ... other fields
}

impl<'h> RegexSetMatches<'h> {
    pub fn disable_pattern(&mut self, index: usize) {
        self.enabled_patterns.remove(index);
        // Invalidate cache for this pattern
    }
    
    pub fn enable_pattern(&mut self, index: usize) {
        self.enabled_patterns.insert(index);
    }
}
```

**Pros:**
- Zero cost for read operations
- No locking required
- Each iterator maintains its own view
- Pattern indices remain stable
- Meets the requirement of modifying during iteration

**Cons:**
- Doesn't actually modify the `RegexSet` itself
- Multiple iterators can have different views
- Still uses DFA for easy patterns (can't selectively disable in DFA)
- Must skip disabled patterns after DFA match

**Performance Impact:** Low
- Bit test per pattern check (very fast)
- No cloning or locking
- DFA still matches disabled patterns, but they're filtered afterward

## Recommended Solution

Given the constraints and requirements, **Solution 5 (Iterator-Local Masking)** is recommended as the best approach:

1. **Meets the stated requirement**: Allows modification between `next()` calls
2. **Low performance cost**: No cloning, no locking, just bit manipulation
3. **Simple implementation**: No complex concurrency primitives
4. **Maintains stability**: Pattern indices don't change

### Implementation Sketch

```rust
use bit_set::BitSet;

pub struct RegexSetMatches<'h> {
    set: &'h RegexSet,
    haystack: &'h str,
    range: Range<usize>,
    current_pos: usize,
    pattern_cache: Vec<Option<(usize, usize, Captures<'h>)>>,
    easy_next_match: Option<(usize, Range<usize>)>,
    pattern_set: Option<PatternSet>,
    enabled_patterns: BitSet,  // NEW: tracks which patterns are enabled
}

impl<'h> RegexSetMatches<'h> {
    /// Disable a pattern by index. Future matches will not include this pattern.
    /// 
    /// # Example
    /// ```
    /// let set = RegexSet::new(&[r"\d+", r"\w+", r"[A-Z]+"])?;
    /// let mut iter = set.matches("abc 123 XYZ");
    /// let first = iter.next()?.unwrap();
    /// // Disable pattern 1
    /// iter.disable_pattern(1);
    /// let second = iter.next()?.unwrap();
    /// // Pattern 1 will not match anymore for this iterator
    /// ```
    pub fn disable_pattern(&mut self, index: usize) {
        if index < self.set.len() {
            self.enabled_patterns.remove(index);
            // Invalidate cache entry for disabled pattern
            if index < self.pattern_cache.len() {
                self.pattern_cache[index] = None;
            }
        }
    }
    
    /// Enable a previously disabled pattern by index.
    pub fn enable_pattern(&mut self, index: usize) {
        if index < self.set.len() {
            self.enabled_patterns.insert(index);
        }
    }
    
    /// Check if a pattern is enabled.
    pub fn is_pattern_enabled(&self, index: usize) -> bool {
        self.enabled_patterns.contains(index)
    }
}
```

In the `next()` implementation, add checks:

```rust
// When considering a match from pattern at index `pattern_id`:
if !self.enabled_patterns.contains(pattern_id) {
    continue; // Skip disabled patterns
}
```

### Alternative: Hybrid Approach

For even more flexibility, we could combine Solution 5 with Solution 2:

1. Use iterator-local masking for disabling patterns during iteration (fast, no cost)
2. Provide `RegexSet::clone_with_patterns(&self, pattern_indices: &[usize])` to create a new set with a subset of patterns (creates a new DFA with only the selected patterns)

This gives users both options:
- Quick filtering during iteration
- Creating optimized sets with fewer patterns for repeated use

## Performance Comparison

| Solution | Read Cost | Write Cost | Complexity | Meets Requirement |
|----------|-----------|------------|------------|-------------------|
| 1. RwLock | High (lock) | High (lock + rebuild) | Medium | Yes* |
| 2. Arc::make_mut | None | Low | Low | No |
| 3. Epoch Counter | Medium (lock) | Medium (lock + rebuild) | High | Yes* |
| 4. Append/Remove End | None | Low | Low | No |
| 5. Iterator Masking | Very Low (bit test) | None | Low | Yes |

\* Requires significant API changes and adds performance overhead

## Conclusion

**Recommended Implementation**: Solution 5 (Iterator-Local Masking)

This approach provides the requested functionality with minimal performance impact and maintains the current clean API. It allows disabling/enabling patterns during iteration without rebuilding the DFA or requiring synchronization primitives.

The limitation that patterns can only be masked (not truly removed from the set) is acceptable since:
1. It's invisible to the user (disabled patterns don't match)
2. The performance is nearly identical
3. The implementation is simple and maintainable
4. Pattern indices remain stable across all iterators
