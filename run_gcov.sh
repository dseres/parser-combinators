#!/bin/sh

rm -rf ./target

export RUSTC_BOOTSTRAP=1
export CARGO_INCREMENTAL=0
export RUSTDOCFLAGS="-Cpanic=abort"
export RUSTFLAGS="-Zprofile -Ccodegen-units=1 -Copt-level=0 -Clink-dead-code -Coverflow-checks=off -Zpanic_abort_tests -Cpanic=abort"

cargo build

cargo test

grcov . -s . --binary-path ./target/debug/ -t html --llvm --branch --ignore-not-existing -o ./target/debug/coverage/
