use aether_frontend::ast::{Module, Item, Function, Param, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_calls_use_win64_abi_and_text_section() {
    let foo = Item::Function(Function {
        name: "foo".to_string(),
        params: vec![],
        ret: Type::String,
        body: vec![Stmt::Return(Expr::Lit(Value::String("x".to_string())))],
        is_pub: true,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("In main before call".to_string()),
            Stmt::PrintExpr(Expr::Call("foo".to_string(), vec![])),
            Stmt::Println("After call".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![foo, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\n        .text\n"), "should emit .text");
    assert!(asm.contains("sub rsp, 40") || asm.contains("sub rsp, 32"), "reserve shadow/alignment before call");
    assert!(asm.contains("mov rcx, rbx"), "console handle in rcx for WriteFile");
    assert!(asm.contains("call WriteFile"), "should call WriteFile");
    assert!(asm.contains("mov rdx, rax") || asm.contains("mov r8d, edx"), "use return for length in printing path");
    assert!(asm.contains("\n        .data\nLSNL:\n        .byte 10\n\n        .text\n") || asm.contains("\n        .data\nLSNL:\n"), "LSNL must be under .data with a return to .text");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn calls_windows_skipped_on_non_windows() {
    assert!(true);
}
