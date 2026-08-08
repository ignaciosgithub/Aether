use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::parse_source;

/// A module mixing a static method call with numeric function-call prints:
/// everything must be emitted in source order by the general emitters.
const SRC: &str = r#"
pub struct Release { title: String, effort: i64 }

static RELEASE: Release = Release { title: "v1", effort: 21 };

pub func Release_kind(self: I64) -> String { return "major"; }

pub func release_effort() -> i64 {
    return RELEASE.effort;
}

pub func main() -> i32 {
    println(RELEASE.kind());
    println(release_effort());
    return 0;
}
"#;

const SELF_FIELD_SRC: &str = r#"
pub struct Rect { w: i64, h: i64 }

pub func Rect_area(self: I64) -> i64 {
    return self.w * self.h;
}

pub func Rect_scaled(self: I64, k: i64) -> i64 {
    return self.w * self.h * k;
}

pub func main() -> i32 {
    let r: Rect = Rect { w: 3, h: 4 };
    println(r.area());
    println(r.scaled(10));
    return 0;
}
"#;

#[test]
fn linux_method_call_in_source_order() {
    let module = parse_source(SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    let kind = asm.find("call Release_kind").expect("method call emitted");
    let effort = asm
        .find("call release_effort")
        .expect("function call emitted");
    assert!(
        kind < effort,
        "method call must precede function call (source order)"
    );
}

#[test]
fn windows_method_call_in_source_order() {
    let module = parse_source(SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    let kind = asm.find("call Release_kind").expect("method call emitted");
    let effort = asm
        .find("call release_effort")
        .expect("function call emitted");
    assert!(kind < effort, "method call must precede function call");
}

#[test]
fn linux_self_field_access() {
    let module = parse_source(SELF_FIELD_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    // self.w / self.h load through the receiver pointer, not zeros.
    assert!(
        asm.contains("mov -8(%rbp), %rax\n        mov 0(%rax), %rax"),
        "expected field load through self pointer: {}",
        asm
    );
    assert!(asm.contains("call Rect_area"));
    assert!(asm.contains("call Rect_scaled"));
}

#[test]
fn windows_self_field_access() {
    let module = parse_source(SELF_FIELD_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("mov rax, qword ptr [rbp-8]\n        mov rax, qword ptr [rax+0]"),
        "expected field load through self pointer: {}",
        asm
    );
    assert!(asm.contains("call Rect_area"));
    assert!(asm.contains("call Rect_scaled"));
}
