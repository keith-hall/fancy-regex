# RegexSet API Specification

## Overview

The `RegexSet` API provides efficient matching of multiple regular expression patterns against the same input text (haystack). This is particularly useful for applications like syntax highlighting, where many patterns need to be matched against each line of text.

The design follows fancy-regex's hybrid approach: patterns are analyzed to determine which can be delegated to high-performance DFA matching ("easy" patterns) and which require backtracking ("hard" patterns). Easy patterns are combined into a single DFA for parallel evaluation, while hard patterns are evaluated individually with optional parallelism using threads.

## Design Goals

1. **Performance**: Minimize overhead for repeated matching against different haystacks (e.g., per-line in syntax highlighting)
2. **Correctness**: Maintain fancy-regex's semantics for all supported features
3. **Priority-based matching**: When multiple patterns match at the same position, the earliest pattern in the set wins
4. **Lazy evaluation**: Only compute what's necessary for each match
5. **Caching**: Reuse match results to avoid redundant computation

## API Structure

### Building a RegexSet

```rust
pub struct RegexSetBuilder {
    patterns: Vec<String>,
    options: RegexOptions,
    max_concurrent_threads: Option<usize>,
}

impl RegexSetBuilder {
    /// Create a new RegexSet builder with a list of patterns.
    pub fn new<I, S>(patterns: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>;

    /// Set case insensitive mode for all patterns in the set.
    pub fn case_insensitive(&mut self, yes: bool) -> &mut Self;

    /// Set multi-line mode for all patterns in the set.
    pub fn multi_line(&mut self, yes: bool) -> &mut Self;

    /// Allow whitespace to be ignored in patterns.
    pub fn ignore_whitespace(&mut self, yes: bool) -> &mut Self;

    /// Enable or disable the "dot matches any character" flag for all patterns.
    pub fn dot_matches_new_line(&mut self, yes: bool) -> &mut Self;

    /// Enable or disable Unicode mode for all patterns.
    pub fn unicode_mode(&mut self, yes: bool) -> &mut Self;

    /// Set the backtracking limit for fancy patterns.
    pub fn backtrack_limit(&mut self, limit: usize) -> &mut Self;

    /// Set size limit for delegated regex compilation.
    pub fn delegate_size_limit(&mut self, limit: usize) -> &mut Self;

    /// Set DFA size limit for delegated regex compilation.
    pub fn delegate_dfa_size_limit(&mut self, limit: usize) -> &mut Self;

    /// Set maximum number of concurrent threads for hard pattern evaluation.
    /// Only applies when the `std` feature is enabled.
    /// 
    /// - `None` (default): Use the number of CPU cores
    /// - `Some(n)`: Use at most `n` threads concurrently
    /// 
    /// When `std` feature is disabled, this setting is ignored and all hard
    /// patterns are evaluated sequentially.
    pub fn max_concurrent_threads(&mut self, limit: Option<usize>) -> &mut Self;

    /// Build the RegexSet.
    /// 
    /// Returns an error if any pattern fails to compile or if resource limits
    /// are exceeded during compilation.
    pub fn build(&self) -> Result<RegexSet>;
}

pub struct RegexSet {
    // Internal structure (not exposed)
}

impl RegexSet {
    /// Create a new RegexSet from an iterator of patterns using default options.
    /// 
    /// All patterns will use the same default configuration:
    /// - Case sensitive
    /// - Multi-line mode disabled
    /// - Dot does not match newline
    /// - Unicode mode enabled
    /// 
    /// # Errors
    /// 
    /// Returns an error if any pattern fails to compile.
    pub fn new<I, S>(patterns: I) -> Result<Self>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>;

    /// Returns the number of patterns in the set.
    pub fn len(&self) -> usize;

    /// Returns true if the set contains no patterns.
    pub fn is_empty(&self) -> bool;

    /// Create a new matches iterator for the given haystack.
    /// 
    /// The iterator will find all non-overlapping matches in the haystack,
    /// returning them in order of their start position. When multiple patterns
    /// match at the same position, the pattern with the lowest index wins.
    /// 
    /// # Arguments
    /// 
    /// * `haystack` - The text to search in
    /// 
    /// # Returns
    /// 
    /// An iterator over matches in the haystack.
    pub fn matches<'h>(&self, haystack: &'h str) -> RegexSetMatches<'h>;

    /// Create a new matches iterator with a specific byte range in the haystack.
    /// 
    /// This is useful when you want to search only a portion of the haystack while
    /// still having access to the full text for features like lookbehind/lookahead.
    /// 
    /// # Arguments
    /// 
    /// * `haystack` - The full text
    /// * `range` - The byte range within the haystack to search
    /// 
    /// # Panics
    /// 
    /// Panics if the range is not within bounds or does not fall on UTF-8 boundaries.
    pub fn matches_range<'h>(&self, haystack: &'h str, range: Range<usize>) 
        -> RegexSetMatches<'h>;
}
```

### Match Results

```rust
/// A match from a RegexSet, including the pattern index and capture groups.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegexSetMatch<'h> {
    pattern_index: usize,
    captures: Captures<'h>,
}

impl<'h> RegexSetMatch<'h> {
    /// Returns the index of the pattern that matched.
    pub fn pattern(&self) -> usize;

    /// Returns the start byte offset of the overall match.
    pub fn start(&self) -> usize;

    /// Returns the end byte offset of the overall match.
    pub fn end(&self) -> usize;

    /// Returns the matched text.
    pub fn as_str(&self) -> &'h str;

    /// Returns the range of the overall match.
    pub fn range(&self) -> Range<usize>;

    /// Returns a reference to the captures for this match.
    pub fn captures(&self) -> &Captures<'h>;

    /// Consumes self and returns the captures.
    pub fn into_captures(self) -> Captures<'h>;
}

/// Iterator over matches in a haystack.
/// 
/// The iterator returns non-overlapping matches in order of their start position.
/// After yielding a match at position `pos` with length `len`, the next match
/// will start searching from `pos + max(1, len)`. This prevents infinite loops
/// on zero-width matches while still allowing them to be found.
/// 
/// When multiple patterns match at the same position, the pattern with the
/// lowest index (specified first in the constructor) is returned.
pub struct RegexSetMatches<'h> {
    // Internal state (not exposed)
}

impl<'h> Iterator for RegexSetMatches<'h> {
    type Item = Result<RegexSetMatch<'h>>;

    /// Returns the next match, or None if no more matches exist.
    /// 
    /// Returns an error if:
    /// - A hard pattern exceeds its backtracking limit
    /// - Any other runtime error occurs during matching
    fn next(&mut self) -> Option<Self::Item>;
}
```

## Internal Architecture

### Pattern Classification

During `RegexSetBuilder::build()`, each pattern is analyzed:

1. **Parse** each pattern into an expression tree (`ExprTree`)
2. **Analyze** each expression tree (using existing `analyze::analyze`) to determine:
   - Whether the pattern is "hard" (requires backtracking)
   - Minimum and constant size properties
   - Capture group structure
3. **Optimize** each expression tree (using existing `optimize::optimize`)
4. **Compile** each pattern:
   - Easy patterns: Extract the delegatable regex pattern for combining
   - Hard patterns: Compile to VM instructions (`Prog`)

### Easy Pattern Handling

Easy patterns (those that can be fully delegated) are combined into a single multi-pattern DFA using `regex-automata`:

1. Use `regex_automata::meta::Builder` to create a multi-pattern regex
2. Each pattern gets an internal pattern ID
3. During matching:
   - Use `find_iter()` or similar to get all matches from the DFA
   - The DFA returns pattern IDs and match positions
   - Extract capture groups by re-matching individual patterns at found positions
   - Handle the "explicit capture group 0" fixup if necessary (when optimization rewrote the pattern)

### Hard Pattern Handling

Hard patterns are evaluated individually:

#### Without `std` feature (sequential):
1. For each hard pattern, call the VM with the current search position
2. Find the leftmost match for each pattern
3. Determine the earliest match position across all patterns
4. Return the match from the lowest-indexed pattern at that position

#### With `std` feature (parallel):
1. Create a thread pool with at most `max_concurrent_threads` threads
2. For each hard pattern, spawn a task to find its leftmost match
3. Use a shared result structure to collect matches
4. Implement early termination:
   - If a match is found at the current search position and it's the lowest-indexed pattern that could match there, return immediately
   - "Could match there" means: no lower-indexed pattern (easy or hard) has already been found to match at that position
   - Otherwise, wait for all threads to complete
5. Keep threads alive and reuse them for the next iteration when possible

**Early termination details**:
- When starting iteration at position `pos`, if we find that pattern `i` matches at exactly `pos`:
  - Check if any pattern with index `< i` could also match at `pos`
  - For easy patterns: This is known from the DFA results (already computed)
  - For hard patterns: If pattern `j < i` hasn't been evaluated yet, we cannot terminate early
  - Only terminate early if we're certain pattern `i` is the winner
- Terminated searches should still complete in the background to warm the cache for future iterations
- If the next iteration starts at a different position, cancel and restart those background tasks

**VM introspection for optimization** (optional advanced feature):
The problem statement mentions potentially introspecting the VM to find the earliest possible start position. This is complex and optional:
- Each thread maintains its own VM state with a current position
- Periodically, the main thread could query the minimum position across all running VMs
- If the main thread determines a winner and all running VMs are searching beyond that position, they can be safely cancelled
- However, this adds significant complexity for potentially minor gains
- **Recommendation**: Implement simple cancellation first (cancel when next iteration changes position), add VM introspection only if profiling shows benefit

### Caching Strategy

To avoid redundant computation across iterations:

```rust
struct MatchCache {
    // For easy patterns: store all matches found by the DFA
    easy_matches: Vec<(usize, Range<usize>, Captures)>, // (pattern_id, range, captures)
    
    // For hard patterns: store the next match for each pattern
    hard_matches: Vec<Option<(Range<usize>, Captures)>>, // indexed by pattern ID
    
    // Position up to which we've searched
    searched_up_to: usize,
}
```

Cache invalidation rules:
- When starting a new iteration at position `pos`:
  - Keep easy matches where `match.start >= pos`
  - Keep hard matches where `match.start >= pos`
  - If any cache is incomplete (we terminated early), mark those entries as invalid

### Match Resolution Algorithm

For each iteration:

1. **Check cache**: If we have a cached match at the current position, return it
2. **Search easy patterns** (if not fully cached):
   - If we haven't searched far enough, query the DFA for more matches
   - Store results in cache
3. **Search hard patterns** (if not fully cached):
   - For each hard pattern without a valid cache entry
   - Start search from current position
   - Store result in cache
4. **Find earliest match**:
   - Consider all cached matches (easy and hard) at or after current position
   - Find the earliest position
   - Among patterns matching at that position, pick lowest index
5. **Return match** and advance iterator position

**Detailed algorithm pseudocode**:

```rust
fn next_match(&mut self) -> Option<Result<RegexSetMatch>> {
    loop {
        // Step 1: Check if we have a winner in cache at current position
        if let Some(match) = self.check_cache_at_position(self.current_pos) {
            self.advance_position_after_match(&match);
            return Some(Ok(match));
        }
        
        // Step 2: Search easy patterns if needed
        if !self.easy_cache.is_complete_up_to(self.current_pos + self.search_ahead) {
            match self.search_easy_patterns() {
                Ok(_) => {},
                Err(e) => return Some(Err(e)),
            }
        }
        
        // Step 3: Search hard patterns if needed
        let hard_results = match self.search_hard_patterns() {
            Ok(results) => results,
            Err(e) => return Some(Err(e)),
        };
        
        // Step 4: Find earliest match across all patterns
        let mut earliest_match: Option<(usize, usize, RegexSetMatch)> = None; // (pos, pattern_index, match)
        
        // Check easy patterns
        for (pattern_id, range, captures) in &self.easy_cache.matches {
            if range.start >= self.current_pos {
                let key = (range.start, pattern_id);
                if earliest_match.is_none() || key < (earliest_match.as_ref().unwrap().0, earliest_match.as_ref().unwrap().1) {
                    earliest_match = Some((range.start, *pattern_id, 
                        RegexSetMatch::new(*pattern_id, captures.clone())));
                }
            }
        }
        
        // Check hard patterns
        for (pattern_id, range, captures) in hard_results {
            if range.start >= self.current_pos {
                let key = (range.start, pattern_id);
                if earliest_match.is_none() || key < (earliest_match.as_ref().unwrap().0, earliest_match.as_ref().unwrap().1) {
                    earliest_match = Some((range.start, pattern_id, 
                        RegexSetMatch::new(pattern_id, captures)));
                }
            }
        }
        
        // Step 5: Return match or signal no more matches
        match earliest_match {
            Some((_, _, match_result)) => {
                self.advance_position_after_match(&match_result);
                return Some(Ok(match_result));
            }
            None => {
                // No matches found, we're done
                return None;
            }
        }
    }
}

fn advance_position_after_match(&mut self, match: &RegexSetMatch) {
    let match_len = match.end() - match.start();
    // Advance by at least 1 to handle zero-width matches
    self.current_pos = match.end() + if match_len == 0 { 1 } else { 0 };
    
    // Invalidate cache entries that are now behind us
    self.invalidate_cache_before(self.current_pos);
}
```

This algorithm ensures:
- Matches are returned in order of position
- At each position, the lowest-indexed pattern wins
- Zero-width matches don't cause infinite loops
- Cache is used efficiently to avoid redundant searches

### Thread Management (std feature only)

```rust
struct ThreadPool {
    workers: Vec<Worker>,
    task_queue: Arc<Mutex<VecDeque<Task>>>,
    result_queue: Arc<Mutex<HashMap<usize, MatchResult>>>,
}

struct Task {
    pattern_id: usize,
    prog: Arc<Prog>,
    haystack: Arc<str>,
    range: Range<usize>,
    start_pos: usize,
}

struct MatchResult {
    pattern_id: usize,
    result: Result<Option<(Range<usize>, Captures)>>,
}
```

Thread pool lifecycle:
- Created on first use of iterator
- Reused across iterations on the same haystack
- Dropped when iterator is dropped or when haystack changes

Cancellation:
- When a match at the current position is found from a lower-priority pattern
- Set a cancellation flag that threads check periodically
- Threads that are cancelled discard their results
- Alternative: Let threads complete but ignore results (simpler, slightly less efficient)

## Memory Management

### Preallocated Resources

To make per-haystack iteration cheap:

1. **VM State**: Pre-allocate state structures for hard patterns
   - Each hard pattern gets a reusable `State` struct
   - State includes backtracking stack, save slots, etc.
2. **Capture Storage**: Pre-allocate capture group storage
3. **Cache Structures**: Pre-allocate match caches

### Resource Sharing

```rust
pub struct RegexSet {
    patterns: Arc<RegexSetImpl>,
}

struct RegexSetImpl {
    easy_patterns: Option<EasyPatternSet>,
    hard_patterns: Vec<HardPattern>,
    options: RegexOptions,
    max_concurrent_threads: Option<usize>,
}

struct EasyPatternSet {
    dfa: Regex, // regex-automata multi-pattern regex
    patterns: Vec<EasyPatternInfo>,
}

struct EasyPatternInfo {
    pattern_id: usize,
    original_index: usize, // index in the RegexSet
    extractor: Option<Regex>, // for extracting capture groups
    explicit_capture_group_0: bool,
}

struct HardPattern {
    pattern_id: usize,
    original_index: usize,
    prog: Arc<Prog>,
    n_groups: usize,
}
```

The `RegexSet` struct is cheaply cloneable (only clones an `Arc`), allowing it to be used from multiple threads safely. However, the `RegexSetMatches` iterator is bound to a single haystack and is NOT thread-safe or cloneable.

## Error Handling

### Compilation Errors

- Invalid regex syntax in any pattern
- Resource limits exceeded (pattern too complex)
- Incompatible options (if added in the future)

Return `Error::CompileError` with details about which pattern failed.

### Runtime Errors

- Backtracking limit exceeded in hard pattern
- Internal VM errors

Return `Result<RegexSetMatch>` from iterator, allowing caller to handle or propagate.

## Example Usage

### Basic Usage

```rust
use fancy_regex::{RegexSet, Result};

fn main() -> Result<()> {
    let set = RegexSet::new(&[
        r"\b\w+\b",           // Pattern 0: words
        r"\d+",              // Pattern 1: numbers  
        r"(?<=\$)\d+\.\d+",  // Pattern 2: prices (with lookbehind)
    ])?;

    let text = "The price is $29.99 today";
    
    for result in set.matches(text) {
        let m = result?;
        println!("Pattern {} matched '{}' at {}..{}", 
            m.pattern(), m.as_str(), m.start(), m.end());
    }
    
    Ok(())
}
```

### With Options

```rust
use fancy_regex::{RegexSetBuilder, Result};

fn main() -> Result<()> {
    let set = RegexSetBuilder::new(&[
        r"hello",
        r"world",
    ])
    .case_insensitive(true)
    .multi_line(true)
    .max_concurrent_threads(Some(4))
    .build()?;

    let text = "HELLO\nWORLD";
    
    for result in set.matches(text) {
        let m = result?;
        println!("Pattern {} matched: {}", m.pattern(), m.as_str());
    }
    
    Ok(())
}
```

### Syntax Highlighting Use Case

```rust
use fancy_regex::{RegexSet, RegexSetBuilder, Result};

struct Highlighter {
    patterns: RegexSet,
}

impl Highlighter {
    fn new() -> Result<Self> {
        let patterns = RegexSetBuilder::new(&[
            r"//.*$",                           // Comments
            r#""(?:[^"\\]|\\.)*""#,            // Strings
            r"\b(fn|let|mut|if|else)\b",      // Keywords
            r"\b[0-9]+\b",                     // Numbers
        ])
        .multi_line(false) // Process line by line
        .build()?;
        
        Ok(Self { patterns })
    }
    
    fn highlight_line(&self, line: &str) -> Result<Vec<(usize, &str, &str)>> {
        let mut tokens = Vec::new();
        
        for result in self.patterns.matches(line) {
            let m = result?;
            let token_type = match m.pattern() {
                0 => "comment",
                1 => "string",
                2 => "keyword",
                3 => "number",
                _ => "unknown",
            };
            tokens.push((m.pattern(), token_type, m.as_str()));
        }
        
        Ok(tokens)
    }
}
```

## Performance Characteristics

### Initialization Cost

**RegexSet::new() / build()**:
- O(sum of pattern complexities) - each pattern analyzed and compiled
- Easy patterns: Additional cost to combine into multi-pattern DFA
  - DFA compilation cost grows with pattern count and complexity
  - Typically O(n × m) where n = number of patterns, m = average pattern size
- Hard patterns: Compiled to VM instructions
  - Linear in pattern complexity
- Generally efficient, but not free - should be done once and reused
- **Benchmark target**: 100 patterns should compile in < 100ms on modern hardware

**RegexSet::matches()**:
- O(1) - Creates iterator with minimal state
- Allocates cache structures (can be pooled for reuse)
- Cheap enough to call per-line in syntax highlighting
- **Benchmark target**: < 1μs per call

### Per-Match Cost

**Easy patterns**:
- Amortized O(haystack length) - single DFA pass finds all matches
- Capture extraction: O(number of matches × pattern complexity)
- Best case: All matches found in one pass, subsequent iterations are cache lookups
- **Benchmark target**: 10-100 MB/s scanning speed for simple patterns

**Hard patterns (without `std`)**:
- O(number of hard patterns × backtracking cost)
- Backtracking cost depends on pattern and input
- Sequential evaluation means total cost is sum of all pattern costs
- **Benchmark target**: Should match current single-pattern `Regex` performance

**Hard patterns (with `std`)**:
- O(max backtracking cost) - patterns run in parallel
- Limited by thread pool size and synchronization overhead
- Thread synchronization overhead: ~1-10μs per pattern
- Best case speedup: min(num_hard_patterns, num_threads)
- **Benchmark target**: 2-4x speedup with 4 threads for CPU-bound patterns

### Memory Usage

- **Per RegexSet**: O(sum of compiled pattern sizes)
  - Easy DFA: Typically 100KB - 10MB depending on pattern complexity and count
  - Hard VM programs: Typically 1-10KB per pattern
  - Metadata: ~100 bytes per pattern
- **Per iterator**: O(number of patterns + cached matches)
  - Cache overhead: ~64 bytes per cached match
  - For syntax highlighting with ~20 tokens per line: ~2KB per iterator
- **Per thread** (with `std`): O(VM state + backtracking stack)
  - VM state: ~1KB base + ~16 bytes per capture group
  - Backtracking stack: ~1KB - 1MB depending on pattern complexity and backtracking limit
  
**Memory benchmark targets**:
- 100 simple patterns: < 1MB for RegexSet
- Iterator with 100 cached matches: < 10KB
- Thread pool with 4 threads: < 1MB total

### Cache Effectiveness

Best case (all easy patterns):
- First iteration: Finds all matches in one DFA pass - O(haystack length)
- Subsequent iterations: O(1) lookup in sorted cache
- Speedup: 100-1000x vs. repeated single-pattern matching

Worst case (all hard patterns, no early matches):
- Each iteration requires running all hard patterns
- Cache provides no benefit if matches are sparse
- Speedup: 0-1x (potentially slower due to overhead)

Typical case (mixed patterns):
- Easy patterns cached from first DFA pass
- Hard patterns cached incrementally
- Significant speedup when matches are densely located
- Speedup: 10-100x vs. repeated single-pattern matching

**Cache invalidation frequency**:
- Every match: Invalidate entries before current position
- Zero-width matches: Advance by 1 byte, minimal invalidation
- Long matches: More efficient, fewer cache invalidations per byte

### Scalability

**Pattern count scaling**:
- Easy patterns: DFA size grows roughly linearly with unique pattern elements
  - 10 patterns: ~100KB
  - 100 patterns: ~1-10MB  
  - 1000 patterns: ~10-100MB (may hit resource limits)
- Hard patterns: Linear growth in memory and sequential search time
- **Recommendation**: Test with target pattern count before deployment

**Haystack size scaling**:
- Easy patterns: O(n) where n = haystack length (DFA guarantees linear time)
- Hard patterns: O(n × backtracking cost) - can be exponential in worst case
- Cache memory: O(number of matches) - grows with match density

**Thread count scaling** (with `std`):
- Speedup typically linear up to number of hard patterns
- Diminishing returns beyond ~8 threads due to synchronization overhead
- Overhead becomes significant if pattern execution time < 10μs

## Implementation Notes

### Capture Group Handling

When combining easy patterns into a single DFA:
1. The DFA can only identify which pattern matched and the overall bounds
2. To extract capture groups:
   - Store a separate `Regex` for each easy pattern (with the original pattern string)
   - When a match is found by the DFA at position `range`, re-run that specific pattern at exactly that position
   - Use an anchored search: `Input::new(haystack).range(range.clone()).anchored(Anchored::Yes)`
   - Extract the captures from this second match
   - This is still efficient because:
     - We know exactly where to search (no scanning)
     - The pattern is anchored (no backtracking about where to start)
     - This only happens for patterns that actually matched
3. For patterns without capture groups, this step can be optimized away

Example implementation:
```rust
struct EasyPatternInfo {
    pattern_id: usize,
    original_index: usize,
    // Compiled individual regex for capture extraction
    extractor: Option<Regex>, // None if no capture groups
    explicit_capture_group_0: bool,
}

// When DFA finds a match at range for pattern_id:
fn extract_captures(
    info: &EasyPatternInfo,
    haystack: &str,
    range: Range<usize>,
) -> Result<Captures> {
    if let Some(ref extractor) = info.extractor {
        let input = Input::new(haystack)
            .range(range.clone())
            .anchored(Anchored::Yes);
        
        let mut captures = extractor.create_captures();
        extractor.search_captures(&input, &mut captures)?;
        
        // Handle explicit_capture_group_0 fixup if needed
        if info.explicit_capture_group_0 {
            // Adjust capture group 0 based on capture group 1
            // (This is the optimization mentioned in the problem statement)
        }
        
        Ok(convert_to_fancy_captures(captures))
    } else {
        // No captures, just return the overall match
        Ok(Captures::new_from_match(haystack, range))
    }
}
```

### Explicit Capture Group 0 Fixup

Some optimizations rewrite patterns in a way that requires fixup:
- Example: `(?=pattern)` at the end → rewritten to `(pattern)` with bounds adjusted
- The `explicit_capture_group_0` flag tracks this
- After matching, adjust the overall match bounds from capture group 0 instead of using the DFA's reported bounds

### Regex-Automata API Usage

Build multi-pattern DFA:
```rust
use regex_automata::meta::{Builder, Regex};
use regex_automata::util::syntax::Config as SyntaxConfig;

let patterns = vec!["pattern1", "pattern2", "pattern3"];

// Configure syntax to match RegexOptions
let syntax_config = SyntaxConfig::new()
    .case_insensitive(options.syntaxc.get_case_insensitive())
    .multi_line(options.syntaxc.get_multi_line())
    .dot_matches_new_line(options.syntaxc.get_dot_matches_new_line())
    .unicode(options.syntaxc.get_unicode());

// Build the multi-pattern regex
let dfa = Builder::new()
    .syntax(syntax_config)
    .build_many(&patterns)?;

// Find matches - note that this returns matches in arbitrary order
let haystack = "...";
let mut matches = vec![];
for mat in dfa.find_iter(haystack) {
    let pattern_id = mat.pattern().as_usize();
    let range = mat.start()..mat.end();
    matches.push((pattern_id, range));
}

// Important: Must sort matches by position, then by pattern index for priority
matches.sort_by_key(|(pattern_id, range)| (range.start, *pattern_id));
```

Key considerations:
1. **Match ordering**: The DFA's `find_iter()` returns matches in the order found, which may not be sorted by position
2. **Pattern IDs**: The DFA assigns pattern IDs sequentially (0, 1, 2, ...) matching the input order
3. **Overlapping matches**: The DFA finds all possible matches, including overlapping ones - the iterator must filter these
4. **Configuration**: All patterns in the set share the same syntax configuration from `RegexOptions`

### No Parallel Haystack Iteration

The specification explicitly disallows using the same `RegexSet` to iterate over multiple haystacks in parallel:

```rust
// NOT SUPPORTED - undefined behavior or compile error
let set = RegexSet::new(&["pattern"])?;
let iter1 = set.matches("haystack1");
let iter2 = set.matches("haystack2"); // Should not be allowed while iter1 is active
```

This allows internal optimizations:
- Reuse pre-allocated caches
- Reuse thread pool
- Avoid synchronization overhead

However, the `RegexSet` itself can be cloned (via Arc) and used from multiple threads as long as each creates its own iterator:

```rust
// SUPPORTED
let set = Arc::new(RegexSet::new(&["pattern"])?);
let set1 = Arc::clone(&set);
let set2 = Arc::clone(&set);

thread::spawn(move || {
    for m in set1.matches("haystack1") { /* ... */ }
});
thread::spawn(move || {
    for m in set2.matches("haystack2") { /* ... */ }
});
```

### Thread Safety

- `RegexSet`: `Send + Sync` (can be shared across threads via Arc)
- `RegexSetBuilder`: Not `Send` or `Sync` (build on one thread)
- `RegexSetMatches`: `Send` but not `Sync` (can be moved to another thread but not shared)

## Testing Strategy

### Unit Tests

1. **Pattern classification**: Verify easy vs. hard detection
2. **Priority resolution**: Ensure lowest-index pattern wins on ties
3. **Cache correctness**: Verify cache invalidation and reuse
4. **Capture groups**: Test extraction for easy and hard patterns
5. **Edge cases**: Empty patterns, empty haystack, no matches

### Integration Tests

1. **Mixed patterns**: Combination of easy and hard patterns
2. **Syntax highlighting**: Real-world use case with per-line iteration
3. **Performance**: Benchmark against repeated single-pattern matching
4. **Thread safety**: Multiple threads using cloned RegexSet

### Compliance Tests

1. **Match existing Regex behavior**: Results should match calling each pattern individually
2. **Oniguruma compatibility**: Where applicable, match Oniguruma's semantics

## Future Enhancements

Potential improvements not included in initial implementation:

1. **Streaming API**: Process input in chunks rather than requiring full haystack
2. **Match overlapping**: Option to return overlapping matches
3. **Multiple winners**: Return all patterns that match at a position
4. **DFA-only mode**: Reject hard patterns for guaranteed linear performance
5. **Pattern modification**: Add/remove patterns without rebuilding entire set
6. **Introspection**: Query which patterns are easy vs. hard before building
7. **Progress reporting**: Callback for long-running matches
8. **Adaptive threading**: Dynamically adjust thread count based on pattern complexity
9. **Cancel iteration**: External signal to stop searching
10. **Match scoring**: Allow patterns to specify priority beyond index order

## Implementation Decisions

The following decisions should be followed during implementation:

1. **Duplicate patterns**: Allow duplicate patterns. They get separate indices.
   - This matches the behavior of regex-automata and is more flexible
   - Users can deduplicate if desired

2. **Thread creation failures**: Fall back to sequential evaluation if thread creation fails.
   - Log a warning (if possible) but continue execution
   - Better to be slow than to fail completely

3. **Pattern classification exposure**: Keep easy vs. hard classification internal.
   - This is an implementation detail that may change
   - Add introspection API later if there's demand

4. **Very large pattern sets** (1000+ patterns):
   - Document that compilation time and memory usage scale with pattern count
   - Consider lazy compilation optimization in future versions
   - Initial implementation should handle at least 100 patterns efficiently

5. **Capture groups**: Always extract capture groups in initial implementation.
   - Future optimization: Add `matches_no_captures()` or similar for better performance
   - The capture group API should mirror the existing `Captures` type

6. **Thread cancellation granularity**: Check cancellation flag between VM instructions.
   - Balance responsiveness with performance overhead
   - May need tuning based on benchmarks

7. **Cache size bounds**: No hard limit on cache size initially.
   - Document memory usage characteristics in API docs
   - Monitor real-world usage and add bounds if needed

8. **Backtracking limits**: Use the same limit for all patterns (from builder).
   - Simplifies implementation
   - Per-pattern limits can be added in future if needed

9. **Empty pattern set**: Allow creating a `RegexSet` with zero patterns.
   - `len()` returns 0, `is_empty()` returns true
   - Iterator immediately returns None

10. **Empty patterns**: Allow empty patterns (e.g., `""`).
    - They match at every position
    - May have performance implications, document this behavior

11. **Match at start of next iteration**: After returning a match at position `pos`, the next iteration starts at `pos + max(1, match_len)`.
    - Prevents infinite loops on zero-width matches
    - Matches behavior of existing `Matches` iterator

12. **Lookbehind at start of range**: When using `matches_range()`, lookbehind can access text before the range start.
    - This is why we need the full haystack, not just a slice
    - The range only restricts where matches can start, not what they can observe

## References

- [regex crate's RegexSet](https://docs.rs/regex/latest/regex/struct.RegexSet.html) - inspiration for API design
- [regex-automata documentation](https://docs.rs/regex-automata/latest/regex_automata/) - underlying multi-pattern matching
- [fancy-regex README](../README.md) - hybrid approach explanation
- [TextMate Language Grammars](https://macromates.com/manual/en/language_grammars) - syntax highlighting use case

---

**Version**: 1.0  
**Last Updated**: 2025-12-26  
**Status**: Specification Complete - Ready for Implementation
