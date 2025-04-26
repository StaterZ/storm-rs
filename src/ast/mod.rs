use lalrpop_util::{lalrpop_mod, ErrorRecovery};

mod nodes;
use nodes::*;

lalrpop_mod!(grammar);

pub fn parse(src_in: &str) -> Output {
	let mut errors = Vec::new();
	let parser = grammar::TopParser::new();
	let ast = parser.parse(&mut errors, &src_in).unwrap();

	Output { ast, errors }
}

pub struct Output<'i> {
	pub ast: Node,
	pub errors: Vec<ErrorRecovery<usize, grammar::Token<'i>, AstError>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstError {
	BadNumber(std::num::ParseIntError),
}
