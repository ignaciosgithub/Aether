use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_print_numeric_params_codegen() {
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
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("syscall") || asm.contains("write"), "expected write syscall for printing");
    assert!(asm.contains("cvttsd2si") || asm.contains("cvtsd2si"), "expected float to int conversion for fractional part");
    assert!(asm.contains(".ascii \".\"") || asm.contains("'.'") || asm.contains(", 46"), "expected dot emission for float");
}
