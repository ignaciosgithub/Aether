use aether_frontend::parse_source;

#[test]
fn parses_float_literal_in_return() {
    let src = r#"
        pub func main() -> void {
            return 3.1415;
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    assert_eq!(module.items.len(), 1);
}

#[test]
fn parses_heterogeneous_list_literal() {
    let src = r#"
        pub func main() -> void {
            return [1, 2.0, true];
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    assert_eq!(module.items.len(), 1);
}
