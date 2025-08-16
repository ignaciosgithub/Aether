use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param, StructDef, Field};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_nonmain_assignment_codegen() {
    let sd_inner = Item::Struct(StructDef {
        name: "Inner".into(),
        fields: vec![Field { name: "y".into(), ty: Type::I32 }],
        parent: None,
        is_pub: true,
    });
    let sd_point = Item::Struct(StructDef {
        name: "Point".into(),
        fields: vec![
            Field { name: "x".into(), ty: Type::I32 },
            Field { name: "inner".into(), ty: Type::User("Inner".into()) },
            Field { name: "name".into(), ty: Type::String },
        ],
        parent: None,
        is_pub: true,
    });

    let f = Item::Function(Function {
        name: "f".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Let {
                name: "n".into(),
                ty: Type::I64,
                init: Expr::Lit(Value::Int(1)),
            },
            Stmt::Assign {
                target: Box::new(Expr::Var("n".into())),
                value: Box::new(Expr::BinOp(
                    Box::new(Expr::Var("n".into())),
                    aether_frontend::ast::BinOpKind::Add,
                    Box::new(Expr::Lit(Value::Int(2))),
                )),
            },
            Stmt::Let {
                name: "p".into(),
                ty: Type::User("Point".into()),
                init: Expr::StructLit(
                    "Point".into(),
                    vec![
                        ("x".into(), Expr::Lit(Value::Int(1))),
                        ("inner".into(), Expr::StructLit("Inner".into(), vec![("y".into(), Expr::Lit(Value::Int(2)))])),
                        ("name".into(), Expr::Lit(Value::String("P0".into()))),
                    ],
                ),
            },
            Stmt::Assign {
                target: Box::new(Expr::Field(Box::new(Expr::Var("p".into())), "x".into())),
                value: Box::new(Expr::Lit(Value::Int(5))),
            },
            Stmt::Assign {
                target: Box::new(Expr::Field(Box::new(Expr::Field(Box::new(Expr::Var("p".into())), "inner".into())), "y".into())),
                value: Box::new(Expr::Lit(Value::Int(9))),
            },
            Stmt::Assign {
                target: Box::new(Expr::Field(Box::new(Expr::Var("p".into())), "name".into())),
                value: Box::new(Expr::Lit(Value::String("P1".into()))),
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
        body: vec![
            Stmt::Expr(Expr::Call("f".into(), vec![])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![sd_inner, sd_point, f, main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nf:\n"), "non-main function f should be emitted");
    assert!(asm.contains("push rbp") && asm.contains("mov rbp, rsp"), "prologue present");
    assert!(asm.contains("pop rbp") || asm.contains("\n        leave\n"), "epilogue present");

}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_nonmain_assignment_codegen_skipped() { assert!(true); }
