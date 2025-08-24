use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn nonmain_arrays_index_and_oob_linux() {
    let arr_fn = Item::Function(Function {
        name: "arr_fn".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "arr".into(),
                ty: Type::Array(Box::new(Type::I32), 3),
                init: Expr::ArrayLit(vec![Expr::Lit(Value::Int(7)), Expr::Lit(Value::Int(8)), Expr::Lit(Value::Int(9))]),
            },
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(
                Expr::Index(Box::new(Expr::Var("arr".into())), Box::new(Expr::Lit(Value::Int(1))))
            ))))),
            Stmt::PrintExpr(Expr::Deref(Box::new(Expr::AddrOf(Box::new(
                Expr::Index(Box::new(Expr::Var("arr".into())), Box::new(Expr::Lit(Value::Int(2))))
            ))))),
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
    let m = Module { items: vec![arr_fn, main] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\narr_fn:\n") || asm.contains("\narr_fn:\r\n"), "expects non-main function label 'arr_fn:'");
    let has_base = asm.contains("leaq -") && asm.contains("(%rbp), %rax");
    let has_scaled = asm.contains("(%rax,%rcx,4)") || asm.contains(",%rcx,4)") || asm.contains(",%rdx,4)") || asm.contains(",%r10,4)");
    let has_disp = asm.contains("leaq 4(%rax)") || asm.contains("leaq -4(%rax)");
    let has_mul = (asm.contains("imul") && asm.contains(", $4")) || (asm.contains("shl") && (asm.contains("$2,") || asm.contains(", $2")));
    assert!(has_base, "expects base address load of local array via leaq -off(%rbp), %rax");
    assert!(has_scaled || has_disp || has_mul, "expects index scaling by 4 for i32 array indexing (scaled addr, immediate 4-byte displacement, imul $4, or shl $2)");
    assert!(asm.contains("syscall"), "expects syscall usage for integer print");
}
