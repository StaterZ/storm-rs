use std::{error::Error, fmt::Display};

use color_print::{cformat, cprintln};
use stopwatch::Stopwatch;
use tree_printer;

use crate::Backend;

pub mod source;
pub mod lexer;
pub mod parser;
pub mod semantic_analyzer;
pub mod comptime;
pub mod codegen;
pub mod interactive;
mod debug_print;

#[derive(Debug, clap::Args)]
pub struct Flags {
	/// Also write the optimized LLVM IR to out/test.opt.ll, next to the unoptimized out/test.ll. Only used by the LLVM backend
	#[arg(long = "emit-optimized-ir")]
	pub emit_optimized_ir: bool,
	/// Print the source with line numbers
	#[arg(long = "show-source")]
	pub show_source: bool,
	/// Print the tokens from the lexer
	#[arg(long = "show-tokens")]
	pub show_tokens: bool,
	/// Print the AST after semantic analysis, with symbols linked to their declarations
	#[arg(long = "show-ast")]
	pub show_ast: bool,
	/// Print what's left to run at runtime after compile time evaluation
	#[arg(long = "show-comptime")]
	pub show_comptime: bool,
}

impl Flags {
	/// Turns on every `show_` flag.
	pub fn show_all(&mut self) {
		self.show_source = true;
		self.show_tokens = true;
		self.show_ast = true;
		self.show_comptime = true;
	}
}

#[derive(Debug, strum::AsRefStr)]
pub enum CompilerError {
	LexerFailed,
	AstParserFailed,
	SatParserFailed,
	ComptimeFailed,
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

	// lexing comes first so the source can be highlighted with the tokens
	let mut lex_timer = Stopwatch::start_new();
	let tokens = lexer::lex(&document);
	lex_timer.stop();

	if flags.show_source {
		println!("=== SOURCE ===");
		let highlight = match &tokens {
			Ok(tokens) => tokens,
			Err(err) => &err.tokens, // the tokens before the error
		};
		debug_print::print_source(&document, Some(highlight));
		println!();
	}

	let tokens = match tokens {
		Ok(tokens) => tokens,
		Err(err) => {
			if flags.show_tokens {
				println!("=== TOKENS ===");
				debug_print::print_tokens(&document, &err.tokens);
				println!();
			}
			err.eprint(&document);
			return Err(CompilerError::LexerFailed);
		}
	};

	if flags.show_tokens {
		println!("=== TOKENS ===");
		debug_print::print_tokens(&document, &tokens);
		println!();
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

	let mut sem_timer = Stopwatch::start_new();
	let sem_output = semantic_analyzer::analyze(&mut ast);
	sem_timer.stop();

	// printed once, after analysis links the symbols. when the analysis fails, it's the parser's AST with whatever got linked
	if flags.show_ast {
		println!("=== AST ===");
		println!("{}", tree_printer::Tree::new(&ast)
				.label("Root")
				.format_line(|label, value| cformat!("<green>{}</>: {}", label, value)));
		println!();
	}

	if let Err(errors) = sem_output {
		cprintln!("<red>SEM Failed:</>");
		for (i, err) in errors.iter().enumerate() {
			cprintln!("  <red>{:04}:</> {}", i, err.to_meta(&document));
		}
		println!();
		return Err(CompilerError::SatParserFailed);
	};

	let mut comptime_timer = Stopwatch::start_new();
	let evaluated = comptime::evaluate(&ast);
	comptime_timer.stop();
	let ast = match evaluated {
		Ok(evaluated) => evaluated.ast,
		Err(errors) => {
			cprintln!("<red>COMPTIME Failed:</>");
			for (i, err) in errors.iter().enumerate() {
				cprintln!("  <red>{:04}:</> {}", i, err.to_meta(&document));
			}
			println!();
			return Err(CompilerError::ComptimeFailed);
		},
	};
	if flags.show_comptime {
		println!("=== COMPTIME ===");
		println!("{}", tree_printer::Tree::new(&ast)
				.label("Root")
				.format_line(|label, value| cformat!("<green>{}</>: {}", label, value)));
		println!();
	}

	// === GENERATOR ===
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
	
	println!("lex time: {}", lex_timer);
	println!("ast time: {}", ast_timer);
	println!("sem time: {}", sem_timer);
	println!("cte time: {}", comptime_timer);
	println!("gen time: {}", gen_timer);
	let total_elapsed = lex_timer.elapsed() + ast_timer.elapsed() + sem_timer.elapsed() + comptime_timer.elapsed() + gen_timer.elapsed();
	println!("tot time: {}ms", total_elapsed.as_millis());
	
	Ok(gen_output)
}
