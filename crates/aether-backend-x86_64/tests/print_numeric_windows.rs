use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn windows_print_numeric_params_codegen() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![
            aether_frontend::ast::Param { name: "a".into(), ty: Type::I64 },
            aether_frontend::ast::Param { name: "b".into(), ty: Type::F64 },
        ],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Var("a".into())),
            Stmt::PrintExpr(Expr::Var("b".into())),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("WriteFile"), "expected WriteFile calls");
    assert!(asm.contains("sub rsp, 40") || asm.contains("add rsp, 40"), "expected shadow space management");
    assert!(asm.contains("cvttsd2si") || asm.contains("cvtsd2si"), "expected float conversion");
}
