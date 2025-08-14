use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

fn build_module_with_println_and_return() -> Module {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("Hello".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Function(func)] }
}

#[test]
fn linux_println_codegen_contains_syscall_and_rodata() {
    let mut cg = X86_64LinuxCodegen::new_linux();
    let module = build_module_with_println_and_return();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".global _start"));
    assert!(asm.contains("mov $1, %rax"));
    assert!(asm.contains("mov $1, %rdi"));
    assert!(asm.contains("syscall"));
    assert!(asm.contains(".section .rodata"));
    assert!(asm.contains(".ascii"));
}

#[test]
fn windows_float_return_still_sets_xmm0() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::F64,
        body: vec![
            Stmt::Return(Expr::BinOp(
                Box::new(Expr::Lit(Value::Float64(1.5))),
                BinOpKind::Add,
                Box::new(Expr::Lit(Value::Float64(2.5))),
            )),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("movsd xmm0"));
}
#[test]
fn windows_println_codegen_contains_winapi_calls_and_data() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("Hello Win".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".extern GetStdHandle"));
    assert!(asm.contains(".extern WriteFile"));
    assert!(asm.contains("call GetStdHandle"));
    assert!(asm.contains("call WriteFile"));
    assert!(asm.contains("sub rsp, 40"));
    assert!(asm.contains(".data"));
    assert!(asm.contains(".ascii"));
}
