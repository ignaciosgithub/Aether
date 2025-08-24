use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn list_i32_len_and_index_codegen_linux() {
    let f = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "xs".into(),
                ty: Type::List(Box::new(Type::I32)),
                init: Expr::ArrayLit(vec![
                    Expr::Lit(Value::Int(1)),
                    Expr::Lit(Value::Int(2)),
                    Expr::Lit(Value::Int(3)),
                ]),
            },
            Stmt::PrintExpr(Expr::Call("len".into(), vec![Expr::Var("xs".into())])),
            Stmt::PrintExpr(Expr::Index(Box::new(Expr::Var("xs".into())), Box::new(Expr::Lit(Value::Int(1))))),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains(".rodata"), "expects rodata section for list literal");
    assert!(asm.contains("movq $3, -") || asm.contains("mov $3, -"), "expected len 3 to be stored to the local slot");
    assert!(asm.contains(",%rcx,4)") || asm.contains(",%r10,4)") || asm.contains(",%rdx,4)"), "expected scaled index x4 load for i32");
}
