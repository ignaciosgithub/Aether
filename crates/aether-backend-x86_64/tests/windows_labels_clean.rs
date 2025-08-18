use aether_frontend::ast::*;
use aether_backend_x86_64::*;
use aether_codegen::CodeGenerator;

#[test]
fn windows_labels_do_not_start_with_dot() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call("readln".into(), vec![])),
            Stmt::PrintExpr(Expr::Call("to_int".into(), vec![Expr::Lit(Value::String("123".into()))])),
            Stmt::PrintExpr(Expr::Call("to_int".into(), vec![Expr::Call("readln".into(), vec![])])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let module = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).unwrap();

    for line in asm.lines() {
        if let Some(stripped) = line.strip_suffix(':') {
            let s = stripped.trim();
            if s.starts_with('.') {
                if s == ".text" || s == ".data" || s == ".bss" || s.starts_with(".section") || s.starts_with(".rodata") {
                    continue;
                }
                panic!("Windows label starts with a dot: {}", s);
            }
        }
    }
}
