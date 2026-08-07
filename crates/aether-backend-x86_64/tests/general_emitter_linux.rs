use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::parse_source;

fn gen(src: &str) -> String {
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    cg.generate(&module).expect("codegen ok")
}

#[test]
fn general_emitter_preserves_source_order_of_prints() {
    let asm = gen(
        r#"
        func work(n: i64) -> i64 {
            let s: i64 = 0;
            let i: i64 = 0;
            while (i < n) {
                s = s + i;
                i = i + 1;
            }
            println("first");
            println(s);
            println("second");
            return s;
        }
        pub func main() -> i32 {
            work(10);
            return 0;
        }
    "#,
    );
    // work() is handled by the general emitter.
    assert!(asm.contains(".LG_WH_work_"), "expected general emitter while labels");
    // String labels are emitted in source order: "first" before "second".
    let rodata: Vec<usize> = ["first", "second"]
        .iter()
        .map(|s| asm.find(&format!(".ascii \"{}", s)).expect("string in rodata"))
        .collect();
    assert!(rodata[0] < rodata[1], "strings must keep source order");
}

#[test]
fn general_emitter_unique_labels_across_functions() {
    let asm = gen(
        r#"
        func a(n: i64) -> i64 {
            let i: i64 = 0;
            while (i < n) { i = i + 1; }
            return i;
        }
        func b(n: i64) -> i64 {
            let i: i64 = 0;
            while (i < n) { i = i + 2; }
            return i;
        }
        pub func main() -> i32 {
            a(3);
            b(3);
            return 0;
        }
    "#,
    );
    let mut labels: Vec<&str> = asm
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with(".LG_") && l.ends_with(':'))
        .collect();
    let total = labels.len();
    labels.sort();
    labels.dedup();
    assert_eq!(total, labels.len(), "duplicate general emitter labels found");
    assert!(total >= 4, "expected while labels for both functions");
}

#[test]
fn general_emitter_handles_recursion_without_patterns() {
    // A recursive shape that no legacy pattern detector matches.
    let asm = gen(
        r#"
        func weird(n: i64) -> i64 {
            return if (n < 3) { n * 7 } else { weird(n - 1) + weird(n - 3) * 2 };
        }
        pub func main() -> i32 {
            println(weird(10));
            return 0;
        }
    "#,
    );
    assert!(asm.contains("call weird"), "expected recursive call");
    assert!(asm.contains(".LG_"), "expected general emitter output");
}
