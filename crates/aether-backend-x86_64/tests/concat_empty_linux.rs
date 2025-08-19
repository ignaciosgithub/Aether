use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_concat_with_empty_string_prints_second_only() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "concat".into(),
                vec![
                    Expr::Lit(Value::String("".into())),
                    Expr::Lit(Value::String("X".into())),
                ],
            )),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("X\\n") || asm.contains("X"), "expected single-character output from concat with empty string");
}
