use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

fn module_nested() -> Module {
    let inner = StructDef {
        name: "Inner".to_string(),
        fields: vec![
            StructField { name: "x".to_string(), ty: Type::I64 },
            StructField { name: "y".to_string(), ty: Type::I32 },
        ],
        parent: None,
    };
    let outer = StructDef {
        name: "Outer".to_string(),
        fields: vec![
            StructField { name: "a".to_string(), ty: Type::I32 },
            StructField { name: "inner".to_string(), ty: Type::User("Inner".to_string()) },
            StructField { name: "s".to_string(), ty: Type::String },
        ],
        parent: None,
    };
    let g = StaticVar {
        name: "G".to_string(),
        ty: Type::User("Outer".to_string()),
        init: Expr::StructLit("Outer".to_string(), vec![
            ("a".to_string(), Expr::Lit(Value::Int(1))),
            ("inner".to_string(), Expr::StructLit("Inner".to_string(), vec![
                ("x".to_string(), Expr::Lit(Value::Int(9))),
                ("y".to_string(), Expr::Lit(Value::Int(7))),
            ])),
            ("s".to_string(), Expr::Lit(Value::String("Hi".to_string()))),
        ]),
    };
    let mainf = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(
                Expr::Field(Box::new(Expr::Var("G".to_string())), "s".to_string())
            ),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Struct(inner), Item::Struct(outer), Item::Static(g), Item::Function(mainf)] }
}

#[test]
fn windows_static_string_field_prints_at_entry() {
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module_nested()).expect("codegen ok");
    assert!(asm.contains("GetStdHandle"), "should get console handle");
    assert!(asm.contains("WriteFile"), "should call WriteFile to print");
    assert!(asm.contains("[rip+G]"), "should take base address of G");
    assert!(asm.contains("qword ptr [r") && asm.contains("+") && asm.contains("]"), "should load ptr from struct field");
    assert!(asm.contains("dword ptr [r") && asm.contains("+") && asm.contains("]"), "should load len from struct field");
}
