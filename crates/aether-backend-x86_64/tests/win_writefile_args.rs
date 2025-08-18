use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_writefile_arg_order_and_shadow_space() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("hello".into()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();

    assert!(asm.contains("mov rcx, r12"), "WriteFile must use rcx=r12");
    assert!(asm.contains("sub rsp, 40"), "must allocate shadow space before WriteFile");
    assert!(asm.contains("call WriteFile"), "must call WriteFile");
    assert!(asm.contains("add rsp, 40"), "must free shadow space after WriteFile");
    let has_len_in_r8d = asm.contains("mov r8d,") || asm.contains("mov r8d, ");
    assert!(has_len_in_r8d, "length must be in r8d");
    let has_ptr_in_rdx = asm.contains("mov rdx,") || asm.contains("lea rdx,");
    assert!(has_ptr_in_rdx, "buffer pointer must be in rdx");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_writefile_arg_order_and_shadow_space_skipped() {
    assert!(true);
}
