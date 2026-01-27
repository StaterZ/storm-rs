use std::{ops::Range, path::Path};

use ariadne::{ColorGenerator, Label, Report, ReportKind};
use itertools::Itertools;
use lalrpop_util::{lalrpop_mod, ErrorRecovery, ParseError};

mod node_sets;
use node_sets::*;
mod nodes;
use nodes::*;

use crate::lexer::{Lexer, Token};

lalrpop_mod!(grammar);

pub fn parse(src_in: &str) -> Output {
	let mut errors = Vec::new();
	let parser = grammar::TopParser::new();
	let lexer = Lexer::new(&src_in);
	let ast = parser.parse(&mut errors, lexer);
	let path = "data/in.storm".to_string(); //TODO

	match ast {
		Err(err) => {
			let mut colors = ColorGenerator::new();
			let report = match err {
				ParseError::UnrecognizedToken { token, expected } => Report::build(
					ReportKind::Error,
					FileSpan::new(path.clone(), token.0..token.2))
					.with_code(3)
					.with_message("Parse error.")
					.with_label(
						Label::new(FileSpan::new(path.clone(), token.0..token.2))
							.with_message(format!("unrecognized token '{:?}'", token.1))
							.with_color(colors.next()))
					.with_note(format!(
						"expected one of the following: {}",
						expected.iter().join(", ")))
					.with_label(
						Label::new(FileSpan::new(
							path.clone(),
							(token.0.saturating_sub(10))..(token.2 + 10),
						))
						.with_message("There was a problem parsing part of this code."))
					.finish(),
				err => Report::build(
					ReportKind::Error,
					FileSpan::new(path.clone(), 0..0))
					.with_message(format!("OUF! {:?}", err))
					.finish()
			};
			report.eprint(ariadne::FnCache::new(|x: &String| {
				std::fs::read_to_string(Path::new(x.as_str()))
			})).expect("failed to print to stderr");
			panic!();
		},
		Ok(ast) => Output { ast, errors },
	}
}

pub struct Output {
	pub ast: Expr,
	pub errors: Vec<ErrorRecovery<usize, Token, AstError>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstError {
	BadNumber(std::num::ParseIntError),
}

#[derive(Debug, Clone)]
pub struct FileSpan {
	pub span: Range<usize>,
	pub path: String,
}

impl FileSpan {
	pub fn new(path: String, span: Range<usize>) -> Self {
		Self { path, span }
	}
}

impl ariadne::Span for FileSpan {
	type SourceId = String;

	fn source(&self) -> &Self::SourceId {
		&self.path
	}

	fn start(&self) -> usize {
		self.span.start
	}

	fn end(&self) -> usize {
		self.span.end
	}
}
