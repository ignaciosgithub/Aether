fn fact(n: i64) -> i64 {
    if n <= 1 { 1 } else { n * fact(n - 1) }
}

fn main() {
    let n: i64 = 12;
    let _x = fact(n);
}
