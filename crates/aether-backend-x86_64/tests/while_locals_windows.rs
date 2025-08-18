use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_while_updates_local_on_stack() {
    let f = Item::Function(Function {
        name: "f".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let { name: "i".into(), ty: Type::I64, init: Expr::Lit(Value::Int(3)) },
            Stmt::While {
                cond: Expr::BinOp(Box::new(Expr::Lit(Value::Int(0))), BinOpKind::Lt, Box::new(Expr::Var("i".into()))),
                body: vec![
                    Stmt::Assign {
                        target: Expr::Var("i".into()),
                        value: Expr::BinOp(Box::new(Expr::Var("i".into())), BinOpKind::Sub, Box::new(Expr::Lit(Value::Int(1)))),
                    },
                ],
            },
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![f] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("LWH_HEAD_f_0:"), "missing while head");
    let has_load = asm.contains("mov rax, qword ptr [rbp-") || asm.contains("mov eax, dword ptr [rbp-");
    let has_store = asm.contains("mov qword ptr [rbp-") || asm.contains("mov dword ptr [rbp-");
    assert!(has_load && has_store, "expected stack load/store for local inside loop");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_on_non_windows() { assert!(true); }
