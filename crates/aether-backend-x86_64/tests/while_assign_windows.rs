use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, BinOpKind};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn windows_nonmain_while_var_increment_codegen() {
    let helper = Item::Function(Function {
        name: "helper".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "i".into(), ty: Type::I64, init: Expr::Lit(Value::Int(0)) },
            Stmt::While {
                cond: Expr::BinOp(
                    Box::new(Expr::Var("i".into())),
                    BinOpKind::Lt,
                    Box::new(Expr::Lit(Value::Int(2))),
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
        is_pub: false,
        is_threaded: false,
    });

    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![ Stmt::Return(Expr::Call("helper".into(), vec![])) ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![helper, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nhelper:\n"), "expected helper symbol");
    assert!(asm.contains("LWH_HEAD_helper_0"), "expected while head label in helper");
    assert!(asm.contains("LWH_END_helper_0"), "expected while end label in helper");
    let has_add = asm.contains("mov rax, qword ptr [rbp-") && asm.contains("add rax, 1") && asm.contains("mov qword ptr [rbp-");
    let has_add32 = asm.contains("mov eax, dword ptr [rbp-") && asm.contains("add eax, 1") && asm.contains("mov dword ptr [rbp-");
    assert!(has_add || has_add32, "expected load/add/store for i = i + 1 in helper loop body");
    assert!(asm.contains("jmp LWH_HEAD_helper_0"), "expected backedge to loop head");
}
