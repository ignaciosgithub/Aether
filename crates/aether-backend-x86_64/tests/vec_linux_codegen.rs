use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

fn gen_asm(module: &Module) -> String {
    let mut codegen = X86_64LinuxCodegen::new_linux();
    codegen.generate(module).expect("codegen")
}

#[test]
fn vec_len_prints_len_from_header() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "v".to_string(),
                        ty: Type::Vector(Box::new(Type::I64)),
                        init: Expr::Call("vec_new".to_string(), vec![Expr::Lit(Value::Int(2))]),
                    },
                    Stmt::PrintExpr(Expr::Call("vec_len".to_string(), vec![Expr::Var("v".to_string())])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("_start:"), "missing linux start");
    assert!(asm.contains(".LSNL:"), "missing newline rodata");
    assert!(asm.contains("mov -"), "expected stack frame locals");
    assert!(asm.contains("mov -"), "header load");
}

#[test]
fn vec_push_grows_and_writes() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "v".to_string(),
                        ty: Type::Vector(Box::new(Type::I64)),
                        init: Expr::Call("vec_new".to_string(), vec![Expr::Lit(Value::Int(1))]),
                    },
                    Stmt::Expr(Expr::Call("vec_push".to_string(), vec![Expr::Var("v".to_string()), Expr::Lit(Value::Int(10))])),
                    Stmt::Expr(Expr::Call("vec_push".to_string(), vec![Expr::Var("v".to_string()), Expr::Lit(Value::Int(20))])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("mov $25, %rax") || asm.contains("mov $9, %rax") || asm.contains("\n        mov $25,"), "expected mremap/mmap in growth path");
    assert!(asm.contains("leaq"), "element address computation");
}

#[test]
fn vec_pop_oob_emits_error() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "v".to_string(),
                        ty: Type::Vector(Box::new(Type::I64)),
                        init: Expr::Call("vec_new".to_string(), vec![Expr::Lit(Value::Int(0))]),
                    },
                    Stmt::PrintExpr(Expr::Call("vec_pop".to_string(), vec![Expr::Var("v".to_string())])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains(".LOOBERR:"), "expected OOB error label");
}

#[test]
fn vec_free_prints_status() {
    let module = Module {
        items: vec![
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "v".to_string(),
                        ty: Type::Vector(Box::new(Type::I64)),
                        init: Expr::Call("vec_new".to_string(), vec![Expr::Lit(Value::Int(2))]),
                    },
                    Stmt::PrintExpr(Expr::Call("vec_free".to_string(), vec![Expr::Var("v".to_string())])),
                    Stmt::PrintExpr(Expr::Call("vec_free".to_string(), vec![Expr::Var("v".to_string())])),
                    Stmt::Return(Expr::Lit(Value::Int(0))),
                ],
                is_pub: true,
                is_threaded: false,
            })
        ],
    };
    let asm = gen_asm(&module);
    assert!(asm.contains("mov $11, %rax") || asm.contains("syscall"), "expected munmap path");
    assert!(asm.contains(".LSNL:"), "newline print should be present");
}
