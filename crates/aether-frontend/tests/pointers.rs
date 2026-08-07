use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parse_addr_of_and_deref() {
    let src = r#"
        func main() -> i32 {
            let x: i64 = 1;
            let p: &i64 = &x;
            let y: i64 = *p;
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}

#[test]
fn parse_deref_assignment() {
    use aether_frontend::ast::{Expr, Item, Stmt};
    let src = r#"
        func main() -> i32 {
            let x: i64 = 1;
            let p: &i64 = &x;
            *p = 42;
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks).unwrap();
    let Item::Function(f) = &module.items[0] else {
        panic!("expected function")
    };
    let Stmt::Assign { target, .. } = &f.body[2] else {
        panic!("expected assign")
    };
    assert!(matches!(target, Expr::Deref(inner) if matches!(&**inner, Expr::Var(n) if n == "p")));
}

#[test]
fn parse_indexing_with_pointers_arrays() {
    let src = r#"
        func main() -> i32 {
            let a: [i32; 4] = [1,2,3,4];
            let p: &i32 = &a[2];
            let v: i32 = *p;
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}
