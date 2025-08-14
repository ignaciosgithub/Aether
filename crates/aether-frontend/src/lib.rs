pub mod ast;
pub mod lexer;
pub mod parser;

use anyhow::Result;

pub fn parse_source(src: &str) -> Result<ast::Module> {
    let tokens = lexer::Lexer::tokenize(src)?;
    parser::Parser::parse(&tokens)
}
