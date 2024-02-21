use std::{error::Error, fmt::Display};

use color_print::cprintln;

use crate::tree_printer;

pub mod source;
pub mod lexer;
pub mod ast;
pub mod sat;
pub mod generator;

mod stream;

type ResultSH<T, E> = Result<Result<T, E>, E>;

#[derive(Debug)]
pub struct Flags {
	pub show_source: bool,
	pub show_tokens: bool,
	pub show_ast: bool,
	pub show_sat: bool,
	pub show_output: bool,
}

#[derive(Debug, strum::AsRefStr)]
pub enum CompilerError {
	LexerFailed,
	AstParserFailed,
	SatParserFailed,
	GeneratorFailed,
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref()) //TODO? make it less crap?
	}
}

impl Error for CompilerError { }

pub fn compile(src_doc: &source::Document, flags: Flags) -> Result<String, CompilerError> {
	if flags.show_source {
		println!("=== Source ===");
		println!("{:?}", src_doc.get_content());
		println!();
	}

	let tokens = match lexer::lex(src_doc) {
		Ok(tokens) => tokens,
		Err(err) => {
			let err_meta = err.with_meta(&src_doc);
			cprintln!("<red>Lexer Failed:</>\n{}", err_meta);
			return Err(CompilerError::LexerFailed);
		}
	};

	if flags.show_tokens {
		println!("=== TOKENS ===");
		for token in tokens.iter() {
			println!("{}", token.with_meta(&src_doc));
		}
		println!();
	}
	
	let ast_root = match ast::parse_ast(&tokens) {
		Ok(root) => root,
		Err(err) => {
			let err_meta = err.to_meta(&src_doc);
			cprintln!("<red>AST Failed:</>\n{}", err_meta);
			return Err(CompilerError::AstParserFailed);
		},
	};
	
	if flags.show_ast {
		println!("=== AST ===");
		tree_printer::print_tree("Root", &ast_root);
		println!();
	}
	
	let sat_root = match sat::parse_sat(&ast_root) {
		Ok(root) => root,
		Err(err) => {
			cprintln!("<red>SAT Failed:</>\n{:?}", err);
			return Err(CompilerError::SatParserFailed);
		},
	};

	
	if flags.show_sat {
		println!("=== SAT ===");
		println!("\t{}", sat_root.as_ref());
		println!();
	}
	
	let gen_output = match generator::generate(&sat_root) {
		Ok(output) => output,
		Err(err) => {
			cprintln!("<red>Generator Failed:</>\n{}", err.as_ref());
			return Err(CompilerError::GeneratorFailed);
		},
	};

	if flags.show_output {
		println!("=== GENERATOR ===");
		println!("\t{}", gen_output);
		println!();
	}

	return Ok(gen_output);
}
