use aether_frontend::parse_source;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn x86_64_linux_emits_xmm0_for_float_return() {
    let src = r#"
        pub func main() -> f64 {
            return 1.5 + 2.25 * 4.0;
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".LC0"), "expected rodata label for f64 constant");
    assert!(asm.contains(".section .rodata") || asm.contains(".data"), "expected data section");
    assert!(asm.contains("movsd"), "expected movsd into xmm0");
}
