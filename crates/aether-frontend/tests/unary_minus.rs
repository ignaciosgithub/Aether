use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::{Expr, Value, BinOpKind};

#[test]
fn parses_negative_literal() {
    let src = "func main()->I32 { return -1; }";
    let toks = Lexer::tokenize(src).unwrap();
    let m = Parser::parse(&toks).unwrap();
    let func = match &m.items[0] { aether_frontend::ast::Item::Function(f) => f, _ => panic!() };
    if let aether_frontend::ast::Stmt::Return(e) = &func.body[0] {
        match e {
            Expr::Lit(Value::Int(v)) => {
                assert_eq!(*v, -1);
            }
            _ => panic!("expected folded literal -1"),
        }
    } else {
        panic!("expected return");
    }
}

#[test]
fn unary_minus_precedence_over_mul() {
    let src = "func main()->I32 { return -2 * 3; }";
    let toks = Lexer::tokenize(src).unwrap();
    let m = Parser::parse(&toks).unwrap();
    let func = match &m.items[0] { aether_frontend::ast::Item::Function(f) => f, _ => panic!() };
    if let aether_frontend::ast::Stmt::Return(e) = &func.body[0] {
        match e {
            Expr::BinOp(l, BinOpKind::Mul, r) => {
                match (&**l, &**r) {
                    (Expr::Lit(Value::Int(two)), Expr::Lit(Value::Int(three))) => {
                        assert_eq!(*two, -2);
                        assert_eq!(*three, 3);
                    }
                    _ => panic!("unexpected AST for -2 * 3"),
                }
            }
            _ => panic!("expected mul at top"),
        }
    } else {
        panic!("expected return");
    }
}
