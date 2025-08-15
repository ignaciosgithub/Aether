use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::*;

#[test]
fn parse_struct_and_static() {
    let src = r#"
    pub struct Point { x: i32, y: i32 }
    static P0: Point = Point { x: 1, y: 2 };
    pub func main() -> i32 { return 0; }
    "#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let module = Parser::parse(&toks).expect("parse ok");
    assert_eq!(module.items.len(), 3);
    match &module.items[0] {
        Item::Struct(sd) => {
            assert_eq!(sd.name, "Point");
            assert_eq!(sd.fields.len(), 2);
            assert_eq!(sd.fields[0].name, "x");
            assert!(matches!(sd.fields[0].ty, Type::I32));
            assert_eq!(sd.fields[1].name, "y");
            assert!(matches!(sd.fields[1].ty, Type::I32));
        }
        _ => panic!("expected struct item"),
    }
    match &module.items[1] {
        Item::Static(st) => {
            assert_eq!(st.name, "P0");
            assert!(matches!(st.ty, Type::User(ref n) if n == "Point"));
            match &st.init {
                Expr::StructLit(name, fields) => {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                }
                _ => panic!("expected struct literal in static init"),
            }
        }
        _ => panic!("expected static item"),
    }
    match &module.items[2] {
        Item::Function(f) => assert_eq!(f.name, "main"),
        _ => panic!("expected function main"),
    }
}

#[test]
fn parse_method_call_and_field_access() {
    let src = r#"
    pub struct Point { x: i32, y: i32 }
    static P1: Point = Point { x: 3, y: 4 };
    pub func main() -> i32 {
        println(P1.x);
        println(P1.name());
        return 0;
    }
    "#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let module = Parser::parse(&toks).expect("parse ok");
    let func = match &module.items[2] {
        Item::Function(f) => f,
        _ => panic!("expected function"),
    };
    assert!(matches!(&func.body[0], Stmt::PrintExpr(_)));
    assert!(matches!(&func.body[1], Stmt::PrintExpr(_)));
    if let Stmt::PrintExpr(e0) = &func.body[0] {
        match e0 {
            Expr::Field(recv, fld) => {
                if let Expr::Var(name) = &**recv {
                    assert_eq!(name, "P1");
                    assert_eq!(fld, "x");
                } else {
                    panic!("expected var receiver");
                }
            }
            _ => panic!("expected field access"),
        }
    }
    if let Stmt::PrintExpr(e1) = &func.body[1] {
        match e1 {
            Expr::MethodCall(recv, meth, args) => {
                if let Expr::Var(name) = &**recv {
                    assert_eq!(name, "P1");
                    assert_eq!(meth, "name");
                    assert!(args.is_empty());
                } else {
                    panic!("expected var receiver");
                }
            }
            _ => panic!("expected method call"),
        }
    }
}
