use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn windows_printexpr_call_concat_prints_and_newline_immediately() {
    let concat_call = Expr::Call("concat".into(), vec![
        Expr::Lit(Value::String("Hi".into())),
        Expr::Lit(Value::String("There".into())),
    ]);
    let mainf = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(concat_call),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![mainf] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let first_wf = asm.match_indices("call WriteFile").map(|(i,_)| i).collect::<Vec<_>>();
    assert!(!first_wf.is_empty(), "expected WriteFile for concat result");
    assert!(asm.contains("lea rdx, [rip+LSNL]"), "expected newline print using LSNL after concat result");
    let wf_positions = first_wf;
    let lsnl_pos = asm.find("lea rdx, [rip+LSNL]").unwrap_or(usize::MAX);
    assert!(wf_positions[0] < lsnl_pos, "newline should be printed after content");

    let ep = asm.find("WMAIN_EPILOG:").unwrap_or(usize::MAX);
    assert!(lsnl_pos < ep, "newline should print before epilogue");
}
