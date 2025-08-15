#![allow(unused_imports)]
use std::hint::black_box;
use std::env;

fn fact(n: i64) -> i64 {
    if n <= 1 { 1 } else { n * fact(n - 1) }
}

fn main() {
    let reps: i64 = env::var("FREPEAT").ok().and_then(|v| v.parse().ok()).unwrap_or(5_000_000);
    let mut i: i64 = 0;
    let mut acc: i64 = 0;
    while i < reps {
        acc += fact(12);
        i += 1;
    }
    println!("{}", acc);
    let _ = black_box(acc);
}
