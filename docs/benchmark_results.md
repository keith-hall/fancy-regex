## Benchmark results (10,000-char haystack of `x`s, pattern `(abc)\1`)

| Benchmark | seek | Time |
|---|---|---|
| `seek_backref_in_long_haystack` | true | ~355 ns |
| `no_seek_backref_in_long_haystack` | false | ~224 µs |
| `seek_backref_in_long_haystack_no_match` | true | ~318 ns |
| `no_seek_backref_in_long_haystack_no_match` | false | ~216 µs |

The seek pre-filter is **~631× faster** in the match case and **~680× faster** in the no-match case. With `seek: true` the engine inlines the captured group body (`abc`) and uses the underlying NFA to jump directly to candidate positions, avoiding running the backtracking VM against every position in the `x`-only prefix.

## Benchmark results (1,000-char haystack of numbers, pattern `(\d{3})\1`)

| Benchmark | seek | Time |
|---|---|---|
| `seek_digit_backref_worst_case` | true | ~112 µs |
| `no_seek_digit_backref_worst_case` | false | ~90 µs |
| `seek_digit_backref_worst_case_no_match` | true | ~112 µs |
| `no_seek_digit_backref_worst_case_no_match` | false | ~91 µs |

Interestingly, the seek pre-filter is **~1.2x slower** in both match and no-match cases. This occurs because with digit patterns like `\d{3}`, there are many potential match positions throughout the haystack (every digit could start a 3-digit sequence), so the seek optimization doesn't significantly reduce the search space. The overhead of checking for the seek condition outweighs the benefits, making the no-seek approach slightly faster in this scenario.

