use lalrpop_util::lalrpop_mod;

pub mod ast;
lalrpop_mod!(#[allow(clippy::all)] grammar);
pub mod lexer;
pub mod parser;
pub mod source;
pub mod token;
