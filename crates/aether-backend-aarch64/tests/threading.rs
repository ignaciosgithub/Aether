use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_aarch64::AArch64Codegen;
use aether_codegen::CodeGenerator;

#[test]
fn aarch64_emits_clone_wait4_kill_and_stack() {
    let m = Module {
        items: vec![
            Item::Function(Function {
                name: "worker".to_string(),
                params: vec![("arg".to_string(), Type::I64)],
                ret_ty: Type::I32,
                body: vec![Stmt::Return(Expr::Lit(Value::Int(0)))],
                is_pub: true,
            }),
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret_ty: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::I64,
                        init: Expr::Call("spawn".to_string(), vec![Expr::Lit(Value::String("worker".to_string())), Expr::Lit(Value::Int(42))]),
                    },
                    Stmt::Let {
                        name: "r".to_string(),
                        ty: Type::I32,
                        init: Expr::Call("join".to_string(), vec![Expr::Var("h".to_string())]),
                    },
                    Stmt::Let {
                        name: "ok".to_string(),
                        ty: Type::I32,
                        init: Expr::Call("destroy".to_string(), vec![Expr::Var("h".to_string())]),
                    },
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
            }),
        ],
        ..Default::default()
    };
    let mut cg = AArch64Codegen::new_linux();
    let asm = cg.generate(&m).expect("aarch64 codegen");
    assert!(asm.contains("mov x8, #220"), "clone syscall");
    assert!(asm.contains("mov x8, #260"), "wait4 syscall");
    assert!(asm.contains("mov x8, #129"), "kill syscall");
    assert!(asm.contains("TSTACK0"), "thread stack in bss/.comm");
}
