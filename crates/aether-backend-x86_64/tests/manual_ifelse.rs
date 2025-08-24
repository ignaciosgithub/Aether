use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn print_asm_for_ifelse_main_params() {
    let func = Function {
        name: "main".to_string(),
        params: vec![
            Param { name: "a".to_string(), ty: Type::I32 },
            Param { name: "b".to_string(), ty: Type::I32 },
        ],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::IfElse {
                cond: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("a".to_string())),
                    BinOpKind::Lt,
                    Box::new(Expr::Var("b".to_string())),
                )),
                then_expr: Box::new(Expr::Lit(Value::String("LT".to_string()))),
                else_expr: Box::new(Expr::Lit(Value::String("GE".to_string()))),
            }),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    println!("=== ASM START ===\n{}\n=== ASM END ===", asm);
    assert!(asm.contains("_start"));
}
