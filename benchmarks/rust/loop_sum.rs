fn sum_to(n: i64) -> i64 {
    if n <= 1 { 1 } else { n + sum_to(n - 1) }
}

fn main() {
    let n: i64 = 20000;
    let _x = sum_to(n);
}
