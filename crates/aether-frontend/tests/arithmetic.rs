use aether_frontend::parse_source;

#[test]
fn parses_arithmetic_with_precedence() {
    let src = r#"
        pub func main() -> void {
            return (5 + 7) * 3 - 4 / 2;
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    assert_eq!(module.items.len(), 1);
}

#[test]
fn parses_mixed_int_float_literals_in_expr() {
    let src = r#"
        pub func main() -> void {
            return 1 + 2.5 * (3 - 1);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    assert_eq!(module.items.len(), 1);
}
