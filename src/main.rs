#![allow(dead_code)]
#![feature(trait_alias)]
#![feature(map_try_insert)]
#![feature(type_alias_impl_trait)]
#![feature(const_trait_impl)]
#![feature(try_blocks)]
#![feature(inplace_iteration)]
#![feature(min_specialization)]
#![feature(anonymous_lifetime_in_impl_trait)]

mod compiler;

use std::path::PathBuf;

use clap::Parser;

use color_print::cprintln;
use compiler::source;
use stopwatch::Stopwatch;

#[derive(Parser, Debug)]
#[clap(author = "StaterZ")]
struct Args {
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

	let Ok(src_in) = std::fs::read_to_string(&args.in_path) else {
		return;
	};

	let src_doc = source::Document::new(
			args.in_path
			.into_os_string()
			.to_string_lossy()
			.to_string(),
		src_in
	);

	let flags = compiler::Flags {
		show_source: false,
		show_tokens: false,
		show_ast_rule_path: false,
		show_ast: false,
		show_sem: false,
		show_output: true,
	};

	let mut timer = Stopwatch::start_new();
	let result = compiler::compile(&src_doc, flags);
	timer.stop();
	println!();
	println!("compile time: {}ms", timer.elapsed().as_millis());
	match &result {
		Ok(_) => cprintln!("<yellow>HAPPY! ^v^</>"),
		Err(_) => cprintln!("<blue>SAD >~<<</>"),
	}
}
