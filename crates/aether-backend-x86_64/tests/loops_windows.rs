use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

#[test]
fn windows_while_emits_labels_and_branches() {
    let func = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Lit(Value::Int(1))),
                    BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(2))),
                ),
                body: vec![
                    Stmt::Println("L".to_string()),
                    Stmt::Continue,
                    Stmt::Break,
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    let module = Module { items: vec![Item::Function(func)] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains(".LWH_HEAD_main_0:") || asm.contains("LWH_HEAD_main_0:"));
    assert!(asm.contains(".LWH_END_main_0:") || asm.contains("LWH_END_main_0:"));
    assert!(asm.contains("cmp r10, r11") || asm.contains("cmp r11, r10"));
    assert!(
        asm.contains("jge .LWH_END_main_0") ||
        asm.contains("jg .LWH_END_main_0") ||
        asm.contains("jl .LWH_HEAD_main_0") ||
        asm.contains("jge LWH_END_main_0") ||
        asm.contains("jg LWH_END_main_0") ||
        asm.contains("jl LWH_HEAD_main_0")
    );
    assert!(asm.contains("jmp .LWH_HEAD_main_0") || asm.contains("jmp LWH_HEAD_main_0"));
}
