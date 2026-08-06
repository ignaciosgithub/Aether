use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, BinOpKind};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn linux_main_while_var_increment_codegen() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "i".into(), ty: Type::I64, init: Expr::Lit(Value::Int(0)) },
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Var("i".into())),
                    BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(3))),
                ),
                body: vec![
                    Stmt::Assign {
                        target: Expr::Var("i".into()),
                        value: Expr::BinOp(
                            Box::new(Expr::Var("i".into())),
                            BinOpKind::Add,
                            Box::new(Expr::Lit(Value::Int(1))),
                        ),
                    }
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");

    // Accept both the legacy (.LWH_*) and general emitter (.LG_WH_*) schemes.
    assert!(asm.contains(".LWH_HEAD_main_0:") || asm.contains(".LG_WH_main_"), "expected while head label");
    assert!(asm.contains(".LWH_END_main_0:") || asm.contains(".LG_WE_main_"), "expected while end label");
    let has_add64 = asm.contains("mov -") && asm.contains("(%rbp), %rax") && (asm.contains("add $1, %rax") || asm.contains("add %rbx, %rax")) && asm.contains("mov %rax, -");
    let has_add32 = asm.contains("mov -") && asm.contains("(%rbp), %eax") && asm.contains("add $1, %eax") && asm.contains("mov %eax, -");
    assert!(has_add64 || has_add32, "expected load/add/store for i = i + 1");
    assert!(asm.contains("jmp .LWH_HEAD_main_0") || asm.contains("jmp .LG_WH_main_"), "expected backedge to loop head");
}
