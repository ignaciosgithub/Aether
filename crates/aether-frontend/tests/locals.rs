use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::*;

#[test]
fn parse_let_and_assign_field() {
    let src = r#"
    pub struct Inner { y: i32 }
    pub struct Point { x: i32, inner: Inner }
    pub func main() -> i32 {
        let p: Point = Point { x: 1, inner: Inner { y: 2 } };
        p.x = 5;
        p.inner.y = 9;
        return 0;
    }
    "#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let module = Parser::parse(&toks).expect("parse ok");
    assert_eq!(module.items.len(), 3);
    let mainf = match &module.items[2] {
        Item::Function(f) => f,
        _ => panic!("expected function"),
    };
    assert!(matches!(&mainf.body[0], Stmt::Let { .. }));
    match &mainf.body[1] {
        Stmt::Assign { target, value: _ } => {
            match target {
                Expr::Field(recv, fname) => {
                    assert_eq!(fname, "x");
                    match &**recv {
                        Expr::Var(v) => assert_eq!(v, "p"),
                        _ => panic!("expected var recv"),
                    }
                }
                _ => panic!("expected field assign"),
            }
        }
        _ => panic!("expected assign stmt"),
    }
    match &mainf.body[2] {
        Stmt::Assign { target, value: _ } => {
            match target {
                Expr::Field(recv, fname) => {
                    assert_eq!(fname, "y");
                    match &**recv {
                        Expr::Field(inner_recv, inner_name) => {
                            assert_eq!(inner_name, "inner");
                            match &**inner_recv {
                                Expr::Var(v) => assert_eq!(v, "p"),
                                _ => panic!("expected var recv"),
                            }
                        }
                        _ => panic!("expected nested field"),
                    }
                }
                _ => panic!("expected nested field assign"),
            }
        }
        _ => panic!("expected assign stmt"),
    }
}
