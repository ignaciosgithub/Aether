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
    // Legacy scheme loads f64 constants from .rodata (.LC0/movsd); the general
    // emitter materializes the bit pattern with movabsq + movq into %xmm0.
    assert!(
        asm.contains(".LC0") || asm.contains("movq %rax, %xmm0"),
        "expected f64 constant load into xmm0"
    );
    assert!(asm.contains("movsd") || asm.contains("mulsd") || asm.contains("addsd"),
            "expected SSE f64 arithmetic");
}
