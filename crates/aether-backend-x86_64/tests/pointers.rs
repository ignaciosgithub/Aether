use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::parse_source;

const SRC: &str = r#"
func add_one(p: &i64) -> i64 {
    *p = *p + 1;
    return *p;
}

pub func main() -> i32 {
    let x: i64 = 10;
    let p: &i64 = &x;
    println(*p);
    *p = 42;
    println(x);
    println(add_one(&x));
    let f: f64 = 2.5;
    let q: &f64 = &f;
    *q = *q * 2.0;
    println(f);
    return 0;
}
"#;

#[test]
fn linux_pointer_codegen() {
    let module = parse_source(SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    assert!(asm.contains("leaq -"), "expected address-of via leaq");
    assert!(asm.contains("mov (%rax), %rax"), "expected int deref load");
    assert!(
        asm.contains("mov %rax, (%rbx)"),
        "expected store through pointer"
    );
    assert!(
        asm.contains("movsd (%rax), %xmm0"),
        "expected float deref load"
    );
    assert!(
        asm.contains("movsd %xmm0, (%rbx)"),
        "expected float store through pointer"
    );
}

#[test]
fn windows_pointer_codegen() {
    let module = parse_source(SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("lea rax, [rbp-"),
        "expected address-of via lea"
    );
    assert!(
        asm.contains("mov rax, qword ptr [rax]"),
        "expected int deref load"
    );
    assert!(
        asm.contains("mov qword ptr [r11], rax"),
        "expected store through pointer"
    );
    assert!(
        asm.contains("movsd xmm0, qword ptr [rax]"),
        "expected float deref load"
    );
    assert!(
        asm.contains("movsd qword ptr [r11], xmm0"),
        "expected float store through pointer"
    );
}

const NESTED_SRC: &str = r#"
pub func main() -> i32 {
    let x: i64 = 1;
    let p: &i64 = &x;
    let pp: &&i64 = &p;
    **pp = 7;
    println(**pp);
    let f: f64 = 2.5;
    let fp: &f64 = &f;
    let fpp: &&f64 = &fp;
    println(**fpp);
    return 0;
}
"#;

#[test]
fn linux_nested_deref_codegen() {
    let module = parse_source(NESTED_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("mov (%rax), %rax\n        mov (%rax), %rax"),
        "expected chained int deref loads"
    );
    assert!(
        asm.contains("movsd (%rax), %xmm0"),
        "expected float load at final deref level"
    );
    assert!(
        asm.contains("mov %rax, (%rbx)"),
        "expected store through nested pointer"
    );
}

#[test]
fn windows_nested_deref_codegen() {
    let module = parse_source(NESTED_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("mov rax, qword ptr [rax]\n        mov rax, qword ptr [rax]"),
        "expected chained int deref loads"
    );
    assert!(
        asm.contains("movsd xmm0, qword ptr [rax]"),
        "expected float load at final deref level"
    );
    assert!(
        asm.contains("mov qword ptr [r11], rax"),
        "expected store through nested pointer"
    );
}

#[test]
fn windows_threads_general_codegen() {
    let src = r#"
pub func worker(arg: i64) -> i32 {
    return (i32)arg;
}

pub func main() -> i32 {
    let h1: i64 = spawn("worker", 101);
    let h2: i64 = spawn("worker", 202);
    let r1: i32 = join(h1);
    let r2: i32 = join(h2);
    let h3: i64 = spawn("worker", 9999);
    let ok: i32 = destroy(h3);
    return r1 + r2;
}
"#;
    let module = parse_source(src).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert_eq!(
        asm.matches("call CreateThread").count(),
        3,
        "one CreateThread per spawn"
    );
    assert_eq!(
        asm.matches("call WaitForSingleObject").count(),
        2,
        "one wait per join"
    );
    assert_eq!(
        asm.matches("call GetExitCodeThread").count(),
        2,
        "join reads the exit code"
    );
    assert_eq!(
        asm.matches("call TerminateThread").count(),
        1,
        "destroy terminates"
    );
    assert_eq!(
        asm.matches("call CloseHandle").count(),
        3,
        "every handle is closed"
    );
    assert!(asm.contains("LG_THK_main_"), "thread thunks emitted");
    // Handles live in stack slots, not global THANDLE data.
    assert!(
        !asm.contains("THANDLE"),
        "no global handle slots in the general emitter"
    );
}
