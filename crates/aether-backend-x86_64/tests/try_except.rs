use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

fn try_module() -> Module {
    let main = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Try {
                body: vec![Stmt::Throw(Expr::Lit(Value::String("oops".into())))],
                err_name: "e".into(),
                handler: vec![Stmt::PrintExpr(Expr::Var("e".into()))],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    Module { items: vec![main] }
}

#[test]
fn try_except_linux_emits_catch_and_message() {
    let m = try_module();
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains(".LCATCH_"), "expects catch label");
    assert!(asm.contains(".LTRYEND_"), "expects try end label");
    assert!(asm.contains("oops"), "expects exception message in data");
}

#[test]
fn try_except_windows_emits_catch_and_message() {
    let m = try_module();
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("WCATCH_"), "expects catch label");
    assert!(asm.contains("WTRYEND_"), "expects try end label");
    assert!(asm.contains("oops"), "expects exception message in data");
}
