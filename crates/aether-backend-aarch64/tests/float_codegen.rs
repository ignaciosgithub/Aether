use aether_frontend::parse_source;
use aether_backend_aarch64::AArch64Codegen;
use aether_codegen::CodeGenerator;

#[test]
fn aarch64_linux_emits_v0_for_float_return() {
    let src = r#"
        pub func main() -> f64 {
            return 1.5 + 2.25 * 4.0;
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".LC0"), "expected rodata label for f64 constant");
    assert!(asm.contains("ldr d0"), "expected load into d0 (v0)");
}
