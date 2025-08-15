#![allow(unused_imports)]
use std::hint::black_box;
use std::env;

fn sum_to_iter(n: i64) -> i64 {
    let mut s: i64 = 0;
    let mut i: i64 = 0;
    while i < n {
        s += i;
        i += 1;
    }
    s
}

fn main() {
    let n: i64 = env::var("N").ok().and_then(|v| v.parse().ok()).unwrap_or(500_000_000);
    let x = sum_to_iter(n);
    println!("{}", x);
    let _ = black_box(x);
}
