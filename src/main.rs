#![allow(dead_code)]
#![feature(trait_alias)]

mod compiler;
mod tree_printer;

use std::path::{Path, PathBuf};

use clap::Parser;

use compiler::source;

#[derive(Parser, Debug)]
#[clap(author = "StaterZ")]
struct Args {
	#[arg(short = 'i', long = "in")]
	in_path: Option<PathBuf>,
	#[arg(short = 'o', long = "out")]
	out_path: Option<PathBuf>,
	#[arg(short = 'd', long = "debug", action = clap::ArgAction::SetTrue)]
	is_debug: bool,
}

fn main() {
	compile();
}

fn compile() {
	let args = Args::parse();

	let path = if args.is_debug {
		Path::new("data/in.txt").to_path_buf()
	} else if let Some(in_path) = args.in_path {
		in_path
	} else {
		println!("No input path");
		return
	};
	
	let Ok(src_in) = std::fs::read_to_string(&path) else {
		return;
	};

	let src_doc = source::Document::new(
		path
			.into_os_string()
			.to_string_lossy()
			.to_string(),
		src_in
	);

	let x = false;
	let flags = compiler::Flags {
		show_source: x,
		show_tokens: x,
		show_ast: x,
		show_sat: x,
		show_output: x,
	};

	let _ = compiler::compile(&src_doc, flags);
}
