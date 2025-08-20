use criterion::{criterion_group, criterion_main, Criterion};
use fancy_regex::{MatchContext, Regex};

fn benchmark_allocation_optimization(c: &mut Criterion) {
    let regex = Regex::new(r"(\w+)\s+\1").unwrap(); // Uses fancy VM features (backreference)
    let text = "hello hello world test test again foo foo bar";
    
    c.bench_function("captures_original", |b| {
        b.iter(|| {
            // Original method - creates new allocations each time
            for _ in 0..10 {
                let _ = regex.captures_from_pos(&text, 0);
            }
        })
    });
    
    c.bench_function("captures_with_context", |b| {
        b.iter(|| {
            // Optimized method - reuses allocations
            let mut ctx = MatchContext::new();
            for _ in 0..10 {
                let _ = regex.captures_from_pos_with_context(&text, 0, &mut ctx);
            }
        })
    });
}

criterion_group!(benches, benchmark_allocation_optimization);
criterion_main!(benches);