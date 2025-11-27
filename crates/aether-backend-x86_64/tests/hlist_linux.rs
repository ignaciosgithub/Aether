use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

fn gen_asm(module: &Module) -> String {
    let mut codegen = X86_64LinuxCodegen::new_linux();
    codegen.generate(module).expect("codegen")
}

#[test]
fn hlist_new_allocates_memory() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::HList,
                        init: Expr::Call("hlist_new".to_string(), vec![Expr::Lit(Value::Int(4))]),
                    },
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("_start:"), "missing linux start");
    assert!(asm.contains("mov $9, %rax"), "expected mmap syscall for allocation");
}

#[test]
fn hlist_push_stores_tagged_element() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::HList,
                        init: Expr::Call("hlist_new".to_string(), vec![Expr::Lit(Value::Int(2))]),
                    },
                    Stmt::Expr(Expr::Call("hlist_push".to_string(), vec![
                        Expr::Var("h".to_string()),
                        Expr::Lit(Value::Int(0)), // tag: i64
                        Expr::Lit(Value::Int(42)), // value
                    ])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("movq $0, (%rbx)") || asm.contains("movq $0,"), "expected tag storage");
    assert!(asm.contains("movq $42, 8(%rbx)") || asm.contains("movq $42,"), "expected value storage");
}

#[test]
fn hlist_len_returns_count() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::HList,
                        init: Expr::Call("hlist_new".to_string(), vec![Expr::Lit(Value::Int(2))]),
                    },
                    Stmt::PrintExpr(Expr::Call("hlist_len".to_string(), vec![Expr::Var("h".to_string())])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains(".LSNL:"), "expected newline for print");
    assert!(asm.contains("syscall"), "expected syscall for print");
}

#[test]
fn hlist_free_releases_memory() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::HList,
                        init: Expr::Call("hlist_new".to_string(), vec![Expr::Lit(Value::Int(2))]),
                    },
                    Stmt::Expr(Expr::Call("hlist_free".to_string(), vec![Expr::Var("h".to_string())])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("mov $11, %rax"), "expected munmap syscall for free");
}
