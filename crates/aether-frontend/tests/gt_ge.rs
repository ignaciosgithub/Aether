use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::*;

#[test]
fn parse_gt_ge_binops() {
    let src = r#"
        func main(a: i32, b: i32) -> i32 {
            return if a > b { 1 } else { 0 };
        }
        func ge(x: i64, y: i64) -> i32 {
            return if x >= y { 1 } else { 0 };
        }
    "#;
    let toks = Lexer::tokenize(src).expect("lex");
    let m = Parser::parse(&toks).expect("parse");
    let mut saw_gt = false;
    let mut saw_ge = false;
    for it in m.items {
        if let Item::Function(f) = it {
            for s in f.body {
                if let Stmt::Return(Expr::IfElse { cond, .. }) = s {
                    if let Expr::BinOp(_, op, _) = *cond {
                        match op {
                            BinOpKind::Gt => saw_gt = true,
                            BinOpKind::Ge => saw_ge = true,
                            _ => {}
                        }
                    }
                }
            }
        }
    }
    assert!(saw_gt, "expected to parse a > b as BinOpKind::Gt");
    assert!(saw_ge, "expected to parse x >= y as BinOpKind::Ge");
}
