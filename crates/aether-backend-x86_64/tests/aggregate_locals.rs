use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::parse_source;

const ARRAY_SRC: &str = r#"
func main() -> i32 {
    let xs: [i32; 4] = [1,2,3,4];
    let i: i32 = 2;
    println("val:");
    println(xs[i]);
    return 0;
}
"#;

const STRUCT_SRC: &str = r#"
struct P { v: i64 }

pub func main() -> i32 {
    let p: P = P { v: 7 };
    println(p.v);
    return 0;
}
"#;

#[test]
fn linux_array_index_print() {
    let module = parse_source(ARRAY_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("movslq (%rbx,%rax,4), %rax"),
        "expected i32 element load: {}",
        asm
    );
    assert!(
        asm.contains("jae .LG_OOB_main"),
        "expected bounds check: {}",
        asm
    );
    assert!(asm.contains(".LG_OOB_main:"), "expected OOB exit path");
}

#[test]
fn windows_array_index_print() {
    let module = parse_source(ARRAY_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("movsxd rax, dword ptr [rbx+rax*4]"),
        "expected i32 element load: {}",
        asm
    );
    assert!(
        asm.contains("jae LG_OOB_main"),
        "expected bounds check: {}",
        asm
    );
    assert!(asm.contains("LG_OOB_main:"), "expected OOB exit path");
}

#[test]
fn linux_struct_local_field_print() {
    let module = parse_source(STRUCT_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("leaq -8(%rbp), %rax"),
        "expected local struct base address: {}",
        asm
    );
    assert!(
        asm.contains("mov 0(%rax), %rax"),
        "expected i64 field load: {}",
        asm
    );
}

#[test]
fn windows_struct_local_field_print() {
    let module = parse_source(STRUCT_SRC).expect("parse");
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen");
    assert!(
        asm.contains("lea rax, [rbp-"),
        "expected local struct base address: {}",
        asm
    );
    assert!(
        asm.contains("mov rax, qword ptr [rax+0]"),
        "expected i64 field load: {}",
        asm
    );
}
