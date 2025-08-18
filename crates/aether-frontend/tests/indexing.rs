use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parse_index_exprs() {
    let src = r#"
        func main() -> i32 {
            let xs: [i32; 3] = [10,20,30];
            let i: i32 = 1;
            let a: i32 = xs[i+1];
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}
