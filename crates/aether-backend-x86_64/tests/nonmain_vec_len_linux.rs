use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_vec_new_len_print_linux() {
    let baz = Item::Function(Function {
        name: "baz".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "v".into(),
                ty: Type::Vector(Box::new(Type::I64)),
                init: Expr::Call("vec_new".into(), vec![Expr::Lit(Value::Int(4))]),
            },
            Stmt::PrintExpr(Expr::Call("vec_len".into(), vec![Expr::Var("v".into())])),
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
    let m = Module { items: vec![baz, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nbaz:\n") || asm.contains("\nbaz:\r\n"), "expects non-main function label 'baz:'");
    assert!(asm.contains("mmap") || asm.contains("syscall"), "expects syscalls for vec runtime or printing");
}
