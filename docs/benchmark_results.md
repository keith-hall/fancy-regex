## Benchmark results (10,000-char haystack of `x`s, pattern `(abc)\1`)

| Benchmark | seek | Time |
|---|---|---|
| `seek_backref_in_long_haystack` | true | ~355 ns |
| `no_seek_backref_in_long_haystack` | false | ~224 µs |
| `seek_backref_in_long_haystack_no_match` | true | ~318 ns |
| `no_seek_backref_in_long_haystack_no_match` | false | ~216 µs |

The seek pre-filter is **~631× faster** in the match case and **~680× faster** in the no-match case. With `seek: true` the engine inlines the captured group body (`abc`) and uses the underlying NFA to jump directly to candidate positions, avoiding running the backtracking VM against every position in the `x`-only prefix.
