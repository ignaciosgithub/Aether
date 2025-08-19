use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;

#[test]
fn windows_inheritance_static_init_and_print_parent_field() {
    let parent = Item::Struct(StructDef {
        name: "Parent".into(),
        fields: vec![StructField { name: "s".into(), ty: Type::String }],
        parent: None,
    });
    let child = Item::Struct(StructDef {
        name: "Child".into(),
        fields: vec![StructField { name: "b".into(), ty: Type::I32 }],
        parent: Some("Parent".into()),
    });
    let cstatic = Item::Static(StaticVar {
        name: "C".into(),
        ty: Type::User("Child".into()),
        init: Expr::StructLit("Child".into(), vec![
            ("s".into(), Expr::Lit(Value::String("Inherit!".into()))),
            ("b".into(), Expr::Lit(Value::Int(42))),
        ]),
    });
    let mainf = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Field(Box::new(Expr::Var("C".into())), "s".into())),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![parent, child, cstatic, mainf] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    assert!(asm.contains("\nC:\n") || asm.contains("\r\nC:\r\n"));
    assert!(asm.contains("Inherit!"));

    assert!(asm.contains("call WriteFile"));
    assert!(asm.contains("lea r10, [rip+C]") || asm.contains("leaq C(%rip), %r10"));
    if let (Some(pos_print), Some(pos_epilog)) = (asm.find("lea r10, [rip+C]"), asm.find("WMAIN_EPILOG")) {
        assert!(pos_print < pos_epilog, "print of C.s must occur before epilogue/ret");
    }
}
