use std::{error::Error, fmt::Display};

use parser::debug::RuleTree;
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

mod map_peekable;

#[derive(Debug)]
pub struct Flags {
	pub show_source: bool,
	pub show_tokens: bool,
	pub show_ast: bool,
	pub show_ast_rule_path: bool,
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
			cprintln!("<cyan>{:>width$} |</> {:?}", line.index(), line.range().get_str());
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
			let err_meta = err.with_meta(&document);
			cprintln!("<red>Lexer Failed:</>\n{}", err_meta);
			return Err(CompilerError::LexerFailed);
		}
	};

	if flags.show_tokens {
		for token in tokens.iter() {
			println!("{}", token.with_meta(&document));
		}
		println!();
	}
	
	if flags.show_ast {
		println!("=== AST ===");
	}
	let mut ast_timer = Stopwatch::start_new();
	let (ast, rule_tree) = if flags.show_ast_rule_path {
		let mut observer = parser::debug::rule_observers::DebugObserver::new();
		(parser::parse(&tokens, &mut observer), Some(observer.conclude()))
	} else {
		(parser::parse(&tokens, &mut parser::debug::rule_observers::DummyObserver { }), None)
	};
	ast_timer.stop();

	fn print_rule_tree(rule_tree: Option<RuleTree>, document: &source::DocumentMeta) {
		if let Some(rule_tree) = &rule_tree {
			tree_printer::print_tree("", &rule_tree.with_meta(&document), |_label, value| cformat!("{}", value))
		}
	}

	let mut ast = match ast {
		Ok(root) => root,
		Err(err) => {
			let err_meta = err.to_meta(&document);
			cprintln!("<red>AST Failed:</>\n{}", err_meta);
			print_rule_tree(rule_tree, &document);
			return Err(CompilerError::AstParserFailed);
		},
	};
	
	print_rule_tree(rule_tree, &document);
	println!();

	if flags.show_ast {
		tree_printer::print_tree("Root", &ast, |label, value| cformat!("<green>{}</>: {}", label, value));
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
		for (i, err) in errors.into_iter().enumerate() {
			cprintln!("  <red>{:04}:</> {}", i, err);
		}
		println!();
		return Err(CompilerError::SatParserFailed);
	};
	if flags.show_sem {
		tree_printer::print_tree("Root", &ast, |label, value| cformat!("<green>{}</>: {}", label, value));
		println!();
	}

	if flags.show_output {
		println!("=== GENERATOR ===");
	}

	let mut gen_timer = Stopwatch::start_new();
	let gen_output = codegen::generate(&ast, backend);
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
