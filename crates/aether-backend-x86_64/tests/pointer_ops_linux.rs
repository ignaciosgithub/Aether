use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn linux_addr_of_and_deref_locals_print() {
    let f = Item::Function(Function {
        name: "foo".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "x".into(), ty: Type::I64, init: Expr::Lit(Value::Int(42)) },
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(Expr::Var("x".into())))))),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("leaq ") && (asm.contains("mov (%rax), %rax") || asm.contains("mov (%rax), %eax")),
        "expected addr-of local via leaq and deref via mov (%rax), %rax/%eax");
}

#[test]
fn linux_addr_of_array_elem_and_deref_print() {
    let f = Item::Function(Function {
        name: "bar".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "arr".into(),
                ty: Type::Array(Box::new(Type::I32), 3),
                init: Expr::ArrayLit(vec![Expr::Lit(Value::Int(7)), Expr::Lit(Value::Int(8)), Expr::Lit(Value::Int(9))]),
            },
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(
                Expr::Index(Box::new(Expr::Var("arr".into())) , Box::new(Expr::Lit(Value::Int(1))))
            ))))),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("leaq ") && (asm.contains("(%rax,%rcx,4)") || asm.contains("(%rax,%rcx,8)") || asm.contains("(%rax)")),
        "expected base leaq and scaled index address computation");
}
