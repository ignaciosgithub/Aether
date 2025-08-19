use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn windows_printexpr_var_string_param_prints_with_newline() {
    let mainf = Item::Function(Function {
        name: "main".into(),
        params: vec![Param { name: "s".into(), ty: Type::String }],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Var("s".into())),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![mainf] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("call WriteFile"));

    assert!(asm.contains("lea rdx, [rip+LSNL]"));
    assert!(asm.contains("mov r8d, 1"));

    let wp = asm.find("call WriteFile").unwrap_or(0);
    let ep = asm.find("WMAIN_EPILOG:").unwrap_or(usize::MAX);
    assert!(wp < ep, "PrintExpr(s) must occur before epilogue");
}
