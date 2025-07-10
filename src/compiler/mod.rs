use std::{error::Error, fmt::Display};

use parser::debug::RuleTree;
use color_print::{cformat, cprintln};
use stopwatch::Stopwatch;
use szu::math::ilog10ceil;
use tree_printer;

pub mod source;
pub mod lexer;
pub mod parser;
pub mod semantic_analyzer;
pub mod generator;

mod map_peekable;

#[derive(Debug)]
pub struct Flags {
	pub show_source: bool,
	pub show_tokens: bool,
	pub show_ast: bool,
	pub show_ast_rule_path: bool,
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
		let width = ilog10ceil(src_doc.get_num_lines());
		println!("=== Source ===");
		for line in src_doc.lines() {
			cprintln!("<cyan>{:>width$} |</> {:?}", line.line.index(), line.range().get_str());
		}
		println!();
	}

	let mut lex_timer = Stopwatch::start_new();
	let tokens = lexer::lex(src_doc);
	lex_timer.stop();
	let tokens = match tokens {
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
	
	let mut ast_timer = Stopwatch::start_new();
	let (ast, rule_tree) = if flags.show_ast_rule_path {
		let mut observer = parser::debug::rule_observers::DebugObserver::new();
		(parser::parse(&tokens, &mut observer), Some(observer.conclude()))
	} else {
		(parser::parse(&tokens, &mut parser::debug::rule_observers::DummyObserver { }), None)
	};
	ast_timer.stop();

	fn print_rule_tree(rule_tree: Option<RuleTree>, src_doc: &source::Document) {
		if let Some(rule_tree) = &rule_tree {
			tree_printer::print_tree("", &rule_tree.with_meta(&src_doc), |_label, value| cformat!("{}", value))
		}
	}

	let mut ast = match ast {
		Ok(root) => root,
		Err(err) => {
			let err_meta = err.to_meta(&src_doc);
			cprintln!("<red>AST Failed:</>\n{}", err_meta);
			print_rule_tree(rule_tree, src_doc);
			return Err(CompilerError::AstParserFailed);
		},
	};
	
	print_rule_tree(rule_tree, src_doc);

	if flags.show_ast {
		println!("=== AST ===");
		tree_printer::print_tree("Root", &ast, |label, value| cformat!("<green>{}</>: {}", label, value));
		println!();
	}
	
	if flags.show_sat {
		println!("=== SEMANTIC ANALYSIS ===");
	}
	let mut sem_timer = Stopwatch::start_new();
	match semantic_analyzer::analyze(&mut ast) {
		Ok(root) => root,
		Err(err) => {
			cprintln!("<red>SEM Failed:</>\n{}", err);
			return Err(CompilerError::SatParserFailed);
		},
	};
	sem_timer.stop();
	println!();

	let mut gen_timer = Stopwatch::start_new();
	let gen_output = generator::generate(&ast);
	gen_timer.stop();
	let gen_output = match gen_output {
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

	println!("lex time: {}", lex_timer);
	println!("ast time: {}", ast_timer);
	println!("sem time: {}", sem_timer);
	println!("gen time: {}", gen_timer);
	return Ok(gen_output);
}
