pub mod ast;
pub mod lexer;
pub mod parser;
mod resolver;

use anyhow::Result;
use std::path::Path;

pub fn parse_source(src: &str) -> Result<ast::Module> {
    let tokens = lexer::Lexer::tokenize(src)?;
    parser::Parser::parse(&tokens)
}

pub fn parse_file_with_imports<P: AsRef<Path>>(path: P) -> Result<ast::Module> {
    resolver::resolve_from_file(path.as_ref())
}
