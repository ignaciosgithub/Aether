use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::{Expr, Stmt, Value};

#[test]
fn parses_try_except_with_throw() {
    let src = r#"pub func main() -> i32 {
        try {
            throw "boom";
        } except (e: String) {
            println(e);
        }
        return 0;
    }"#;
    let toks = Lexer::tokenize(src).unwrap();
    let m = Parser::parse(&toks).unwrap();
    let func = match &m.items[0] { aether_frontend::ast::Item::Function(f) => f, _ => panic!() };
    match &func.body[0] {
        Stmt::Try { body, err_name, handler } => {
            assert_eq!(err_name, "e");
            match &body[0] {
                Stmt::Throw(Expr::Lit(Value::String(s))) => assert_eq!(s, "boom"),
                other => panic!("expected throw, got {:?}", other),
            }
            match &handler[0] {
                Stmt::PrintExpr(Expr::Var(v)) => assert_eq!(v, "e"),
                other => panic!("expected println(e), got {:?}", other),
            }
        }
        other => panic!("expected try stmt, got {:?}", other),
    }
}

#[test]
fn parses_top_level_throw() {
    let src = r#"pub func main() -> i32 {
        throw "fatal";
        return 1;
    }"#;
    let toks = Lexer::tokenize(src).unwrap();
    let m = Parser::parse(&toks).unwrap();
    let func = match &m.items[0] { aether_frontend::ast::Item::Function(f) => f, _ => panic!() };
    match &func.body[0] {
        Stmt::Throw(Expr::Lit(Value::String(s))) => assert_eq!(s, "fatal"),
        other => panic!("expected throw, got {:?}", other),
    }
}
