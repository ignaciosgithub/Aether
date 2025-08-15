use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::*;

#[test]
fn parse_while_with_print_and_break() {
    let src = r#"
    pub func main() -> i32 {
        while (1 < 2) {
            println("X");
            break;
        }
        return 0;
    }
    "#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let module = Parser::parse(&toks).expect("parse ok");
    let func = match &module.items[0] {
        Item::Function(f) => f,
    };
    assert_eq!(func.name, "main");
    match &func.body[0] {
        Stmt::While { cond, body } => {
            match cond {
                Expr::BinOp(a, BinOpKind::Lt, b) => {
                    matches!(&**a, Expr::Lit(Value::Int(_)));
                    matches!(&**b, Expr::Lit(Value::Int(_)));
                }
                _ => panic!("expected lt binop"),
            }
            assert!(matches!(&body[0], Stmt::Println(_)));
            assert!(matches!(&body[1], Stmt::Break));
        }
        _ => panic!("expected while"),
    }
}
