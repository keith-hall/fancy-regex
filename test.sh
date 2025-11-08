#!/bin/bash
set -e

cargo check
cargo check --no-default-features
cargo check --no-default-features --features "unicode,perf,variable-lookbehinds"
cargo check --examples
cargo check --benches

cargo test
cargo test --no-default-features
cargo test --no-default-features --features "unicode,perf,variable-lookbehinds"
cargo run --example toy trace '\d*' '1122 33'
cargo test --examples
