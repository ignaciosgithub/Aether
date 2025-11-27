// Benchmark: Sum of squares
// Computes sum of i*i for i from 0 to n-1, repeated R times

fn work(n: i64) -> i64 {
    let mut s: i64 = 0;
    let mut i: i64 = 0;
    while i < n {
        s = s + i * i;
        i = i + 1;
    }
    s
}

fn main() {
    let mut total: i64 = 0;
    let mut k: i64 = 0;
    let n: i64 = 10000000;
    let r: i64 = 10;
    while k < r {
        total = total + work(n);
        k = k + 1;
    }
    println!("{}", total);
}
