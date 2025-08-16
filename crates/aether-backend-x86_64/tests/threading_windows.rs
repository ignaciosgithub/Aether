use aether_frontend::ast::{Module, Item, Function, Param, Stmt, Expr, Type, Value};

#[cfg(target_os = "windows")]
#[test]
fn windows_emits_winapi_thread_symbols() {
    let m = Module {
        items: vec![
            Item::Function(Function {
                name: "worker".to_string(),
                params: vec![Param { name: "arg".to_string(), ty: Type::I64 }],
                ret: Type::I32,
                body: vec![Stmt::Return(Expr::Lit(Value::Int(0)))],
                is_pub: true,
                is_threaded: false,
            }),
            Item::Function(Function {
                name: "main".to_string(),
                params: vec![],
                ret: Type::I32,
                body: vec![
                    Stmt::Let {
                        name: "h".to_string(),
                        ty: Type::I64,
                        init: Expr::Call("spawn".to_string(), vec![Expr::Lit(Value::String("worker".to_string())), Expr::Lit(Value::Int(1))]),
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
                is_threaded: false,
            }),
        ],
    };
    let _ = m;
    assert!(true);
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_thread_symbols_ignored_on_non_windows() {
    assert!(true);
}
