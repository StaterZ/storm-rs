use std::{error::Error, fmt::Display};

use color_print::{cformat, cprintln};
use stopwatch::Stopwatch;
use szu::math::ilog10ceil;
use tree_printer;

use crate::Backend;

pub mod source;
pub mod lexer;
pub mod parser;
pub mod semantic_analyzer;
pub mod codegen;
pub mod interactive;

#[derive(Debug)]
pub struct Flags {
	pub emit_optimized_ir: bool,
	pub show_source: bool,
	pub show_tokens: bool,
	pub show_ast: bool,
	pub show_sem: bool,
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

pub fn compile(document: &source::Document, backend: Backend, flags: Flags) -> Result<Vec<u8>, CompilerError> {
	let document = source::DocumentMeta::new(document); //TODO: this isn't great, change it to only do this if we find errors

	if flags.show_source {
		println!("=== Source ===");
		let width = ilog10ceil(document.get_num_lines());
		for line in document.lines() {
			cprintln!("<cyan>{:>width$} |</> {:?}", line, line.range().get_str());
		}
		println!();
	}

	if flags.show_tokens {
		println!("=== TOKENS ===");
	}
	let mut lex_timer = Stopwatch::start_new();
	let tokens = lexer::lex(&document);
	lex_timer.stop();
	let tokens = match tokens {
		Ok(tokens) => tokens,
		Err(err) => {
			err.eprint(&document);
			return Err(CompilerError::LexerFailed);
		}
	};

	if flags.show_tokens {
		for token in tokens.iter() {
			let token = token.as_ref().with_meta(&document);
			println!("{:?}", token);
		}
		println!();
	}
	
	if flags.show_ast {
		println!("=== AST ===");
	}
	let mut ast_timer = Stopwatch::start_new();
	let ast = parser::parse(&tokens);
	ast_timer.stop();

	let mut ast = match ast {
		Ok(root) => root,
		Err(err) => {
			let err_meta = err.to_meta(&document);
			cprintln!("<red>AST Failed:</>\n{}", err_meta);
			return Err(CompilerError::AstParserFailed);
		},
	};

	if flags.show_ast {
		println!("{}", tree_printer::Tree::new(&ast)
				.label("Root")
				.format_line(|label, value| cformat!("<green>{}</>: {}", label, value)));
		println!();
	}
	
	if flags.show_sem {
		println!("=== SEMANTIC ANALYSIS ===");
	}
	let mut sem_timer = Stopwatch::start_new();
	let sem_output = semantic_analyzer::analyze(&mut ast);
	sem_timer.stop();
	if let Err(errors) = sem_output {
		cprintln!("<red>SEM Failed:</>");
		for (i, err) in errors.iter().enumerate() {
			cprintln!("  <red>{:04}:</> {}", i, err.to_meta(&document));
		}
		println!();
		return Err(CompilerError::SatParserFailed);
	};
	if flags.show_sem {
		println!("{}", tree_printer::Tree::new(&ast)
				.label("Root")
				.format_line(|label, value| cformat!("<green>{}</>: {}", label, value)));
		println!();
	}

	if flags.show_output {
		println!("=== GENERATOR ===");
	}

	let mut gen_timer = Stopwatch::start_new();
	let gen_output = codegen::generate(&ast, backend, flags.emit_optimized_ir);
	gen_timer.stop();
	let gen_output = match gen_output {
		Ok(output) => output,
		Err(err) => {
			cprintln!("<red>Generator Failed:</>\n{}", err.as_ref());
			return Err(CompilerError::GeneratorFailed);
		},
	};

	if flags.show_output {
		println!("{:?}", gen_output);
		println!();
	}

	println!("lex time: {}", lex_timer);
	println!("ast time: {}", ast_timer);
	println!("sem time: {}", sem_timer);
	println!("gen time: {}", gen_timer);
	let total_elapsed = lex_timer.elapsed() + ast_timer.elapsed() + sem_timer.elapsed() + gen_timer.elapsed();
	println!("tot time: {}ms", total_elapsed.as_millis());
	
	Ok(gen_output)
}
