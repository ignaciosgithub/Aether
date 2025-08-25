use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_vec_ops_linux() {
    let vec_fn = Item::Function(Function {
        name: "vec_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "v".into(),
                ty: Type::Vector(Box::new(Type::I64)),
                init: Expr::Call("vec_new".into(), vec![Expr::Lit(Value::Int(2))]),
            },
            Stmt::PrintExpr(Expr::Call("vec_len".into(), vec![Expr::Var("v".into())])),
            Stmt::Expr(Expr::Call("vec_push".into(), vec![Expr::Var("v".into()), Expr::Lit(Value::Int(7))])),
            Stmt::Expr(Expr::Call("vec_push".into(), vec![Expr::Var("v".into()), Expr::Lit(Value::Int(8))])),
            Stmt::Expr(Expr::Call("vec_pop".into(), vec![Expr::Var("v".into())])),
            Stmt::PrintExpr(Expr::Call("vec_len".into(), vec![Expr::Var("v".into())])),
            Stmt::PrintExpr(Expr::Call("vec_free".into(), vec![Expr::Var("v".into())])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let main = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![Stmt::Return(Expr::Lit(Value::Int(0)))],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![vec_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nvec_fn:\n") || asm.contains("\nvec_fn:\r\n"), "expects non-main function label 'vec_fn:'");
    assert!(asm.contains("syscall"), "expects syscalls for vec runtime (mmap/mremap/munmap) and printing");
}
