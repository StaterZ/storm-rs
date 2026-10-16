#![allow(dead_code)]
#![feature(const_trait_impl)]
#![feature(try_blocks)]
#![feature(min_specialization)]

mod compiler;

use std::path::PathBuf;

use clap::{Parser, ValueEnum};

use color_print::cprintln;
use compiler::source;
use stopwatch::Stopwatch;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Backend {
	LLVM,
	Lua,
	Stormworks,
}

#[derive(Parser, Debug)]
#[clap(author = "StaterZ")]
struct Cli {
	#[arg(
		short = 'b',
		long = "backend",
		required_unless_present = "interactive",
	)]
	#[clap(value_enum)]
	backend: Option<Backend>,

	#[arg(
		short = 'i',
		long = "in-path",
		required_unless_present = "interactive",
	)]
	in_path: Option<PathBuf>,

	#[arg(
		short = 'o',
		long = "out-path"
	)]
	out_path: Option<PathBuf>,

	/// Enter code snippets and see their AST, instead of compiling a file
	#[arg(
		short = 'I',
		long = "interactive",
		conflicts_with_all = ["backend", "in_path", "out_path", "emit_optimized_ir"],
	)]
	interactive: bool,

	/// Also write the optimized LLVM IR to out/test.opt.ll, next to the unoptimized out/test.ll. Only used by the LLVM backend
	#[arg(long = "emit-optimized-ir")]
	emit_optimized_ir: bool,
}

fn main() {
	let args = Cli::parse();
	if args.interactive {
		compiler::interactive::run();
	} else {
		compile(args);
	}
}

fn compile(args: Cli) {
	let backend = args.backend.expect("clap requires a backend outside interactive mode");
	let in_path = args.in_path.expect("clap requires an in-path outside interactive mode");
	let out_path = args.out_path.unwrap_or(in_path.with_extension("out"));

	let content = match std::fs::read_to_string(&in_path) {
		Ok(content) => content,
		Err(err) => {
			eprintln!("Failed to read input file: {}", err);
			return;
		}
	};

	let document = source::Document::new(
			in_path
			.into_os_string()
			.to_string_lossy()
			.to_string(),
		content
	);

	let flags = compiler::Flags {
		emit_optimized_ir: args.emit_optimized_ir,
		show_source: true,
		show_tokens: true,
		show_ast: true,
		show_sem: true,
		show_output: true,
	};

	let mut timer = Stopwatch::start_new();
	let result = compiler::compile(&document, backend, flags);
	timer.stop();
	println!();
	println!("compile time: {}ms", timer.elapsed().as_millis());
	match &result {
		Ok(_) => cprintln!("<yellow>HAPPY! ^v^</>"),
		Err(_) => { cprintln!("<blue>SAD >~<<</>"); return; },
	}

	let Ok(_) = std::fs::write(out_path, result.unwrap()) else {
		return;
	};
}
