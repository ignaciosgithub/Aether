use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_nested_concat_prints_combined_string() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "concat".into(),
                vec![
                    Expr::Lit(Value::String("A".into())),
                    Expr::Call(
                        "concat".into(),
                        vec![
                            Expr::Lit(Value::String("B".into())),
                            Expr::Lit(Value::String("C".into())),
                        ],
                    ),
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
    assert!(asm.contains("ABC\\n") || asm.contains("ABC"), "expected nested concatenated string in rodata/prints");
}
