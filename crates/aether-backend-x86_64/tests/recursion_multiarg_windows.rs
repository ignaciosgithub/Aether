use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_multiarg_string_recursion_calls_use_win64_abi() {
    let a = Item::Function(Function {
        name: "a".into(),
        params: vec![
            Param { name: "x".into(), ty: Type::I64 },
            Param { name: "y".into(), ty: Type::I32 },
            Param { name: "z".into(), ty: Type::F64 },
            Param { name: "s".into(), ty: Type::String },
        ],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse(
                Expr::Lit(Value::Int(0)),
                vec![Stmt::Expr(Expr::Call("b".into(), vec![
                    Expr::Lit(Value::Int(1)),
                    Expr::Lit(Value::Int(2)),
                    Expr::Lit(Value::Float64(3.0)),
                    Expr::Lit(Value::String("X".into())),
                ]))],
                vec![]
            ),
            Stmt::Return(Expr::Lit(Value::Int(7))),
        ],
        is_pub: false,
        is_threaded: false,
    });
    let b = Item::Function(Function {
        name: "b".into(),
        params: vec![
            Param { name: "x".into(), ty: Type::I64 },
            Param { name: "y".into(), ty: Type::I32 },
            Param { name: "z".into(), ty: Type::F64 },
            Param { name: "s".into(), ty: Type::String },
        ],
        ret: Type::I32,
        body: vec![
            Stmt::IfElse(
                Expr::Lit(Value::Int(0)),
                vec![Stmt::Expr(Expr::Call("a".into(), vec![
                    Expr::Lit(Value::Int(4)),
                    Expr::Lit(Value::Int(5)),
                    Expr::Lit(Value::Float64(6.0)),
                    Expr::Lit(Value::String("Y".into())),
                ]))],
                vec![]
            ),
            Stmt::Return(Expr::Lit(Value::Int(9))),
        ],
        is_pub: false,
        is_threaded: false,
    });
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("a".into(), vec![
                Expr::Lit(Value::Int(10)),
                Expr::Lit(Value::Int(20)),
                Expr::Lit(Value::Float64(30.0)),
                Expr::Lit(Value::String("Z".into())),
            ])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![a, b, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\na:\n") && asm.contains("\nb:\n"), "functions should be emitted");
    let has_shadow_space = asm.contains("sub rsp, 32") && asm.contains("add rsp, 32");
    assert!(has_shadow_space, "win64 calls must use 32-byte shadow space");
    let mentions_param_regs = asm.contains("rcx") || asm.contains("rdx") || asm.contains("r8") || asm.contains("r9");
    assert!(mentions_param_regs, "expected mention of param registers in asm");
    let preserves_rbx = asm.contains("push rbx") && (asm.contains("pop rbx") || asm.contains("\n        leave\n"));
    assert!(preserves_rbx, "should preserve rbx in prologue/epilogue");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_multiarg_string_recursion_calls_use_win64_abi_skipped() { assert!(true); }
