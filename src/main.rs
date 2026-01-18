#![allow(dead_code)]
#![feature(trait_alias)]
#![feature(map_try_insert)]
#![feature(type_alias_impl_trait)]
#![feature(const_trait_impl)]
#![feature(try_blocks)]
#![feature(inplace_iteration)]
#![feature(min_specialization)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(box_patterns)]

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
struct Args {
	#[arg(
		short = 'b',
		long = "backend",
	)]
	#[clap(value_enum)]
	backend: Backend,

	#[arg(
		short = 'i',
		long = "in-path",
	)]
	in_path: PathBuf,

	#[arg(
		short = 'o',
		long = "out-path"
	)]
	out_path: Option<PathBuf>,
}

fn main() {
	compile();
}

fn compile() {
	let args = Args::parse();
	let out_path = args.out_path.unwrap_or(args.in_path.with_extension("out"));

	let content = match std::fs::read_to_string(&args.in_path) {
		Ok(content) => content,
		Err(err) => {
			eprintln!("Failed to read input file: {}", err);
			return;
		}
	};

	let document = source::Document::new(
			args.in_path
			.into_os_string()
			.to_string_lossy()
			.to_string(),
		content
	);

	let flags = compiler::Flags {
		show_source: false,
		show_tokens: false,
		show_ast_rule_path: true,
		show_ast: false,
		show_sem: true,
		show_output: false,
	};

	let mut timer = Stopwatch::start_new();
	let result = compiler::compile(&document, args.backend, flags);
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
