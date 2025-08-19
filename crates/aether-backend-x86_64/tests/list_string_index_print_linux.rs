use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn list_string_index_print_linux() {
    let f = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "ss".into(),
                ty: Type::List(Box::new(Type::String)),
                init: Expr::ArrayLit(vec![
                    Expr::Lit(Value::String("a".into())),
                    Expr::Lit(Value::String("bc".into())),
                ]),
            },
            Stmt::PrintExpr(Expr::Index(Box::new(Expr::Var("ss".into())), Box::new(Expr::Lit(Value::Int(1))))),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains(".rodata"), "expects rodata section");
    assert!(asm.contains(".ascii \"a\"") && asm.contains(".ascii \"bc\""), "expects string bytes emitted");
    assert!(asm.contains("leaq .LSNL(%rip), %rsi") || asm.contains("leaq .LSNL(%rip), %rsi"), "expects newline write after printing");
    assert!(asm.contains("syscall"), "expects write syscall usage");
}
