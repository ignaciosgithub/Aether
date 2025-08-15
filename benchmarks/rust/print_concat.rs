use std::env;

fn main() {
    let count: i64 = env::var("PCOUNT").ok().and_then(|v| v.parse().ok()).unwrap_or(200_000);
    let mut i: i64 = 0;
    while i < count {
        println!("hello");
        println!("world");
        println!("aether");
        i += 1;
    }
}
