use aether_frontend::ast::{Module, Item, Function, Param, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

fn module_for_spawn_join_destroy() -> Module {
    Module {
        items: vec![
            Item::Function(Function {
                name: "worker".to_string(),
                params: vec![Param { name: "arg".to_string(), ty: Type::I64 }],
                ret: Type::I32,
                body: vec![
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            }),
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h1".to_string(),
                        ty: Type::I64,
                        init: Expr::Call("spawn".to_string(), vec![Expr::Lit(Value::String("worker".to_string())), Expr::Lit(Value::Int(1))]),
                    },
                    Stmt::Let {
                        name: "r1".to_string(),
                        ty: Type::I32,
                        init: Expr::Call("join".to_string(), vec![Expr::Var("h1".to_string())]),
                    },
                    Stmt::Let {
                        name: "ok".to_string(),
                        ty: Type::I32,
                        init: Expr::Call("destroy".to_string(), vec![Expr::Var("h1".to_string())]),
                    },
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    }
}

#[test]
fn linux_emits_clone_wait4_kill() {
    let m = module_for_spawn_join_destroy();
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("mov $56, %rax") || asm.contains("movq $56, %rax"), "should emit clone syscall");
    assert!(asm.contains("TSTACK0"), "should emit thread stack");
    assert!(asm.contains("mov $61, %rax") || asm.contains("movq $61, %rax"), "should emit wait4 syscall");
    assert!(asm.contains("mov $62, %rax") || asm.contains("movq $62, %rax"), "should emit kill syscall");
}
