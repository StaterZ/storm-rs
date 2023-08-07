#![allow(dead_code)]

use clap::Parser;
use owo_colors::OwoColorize;
use std::path::{Path, PathBuf};
use compiler::source_meta::SourceFile;

mod compiler;
mod tree_printer;

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
	
	if let Ok(src_in) = std::fs::read_to_string(path) {
		println!("=== Source ===");
		println!("{}", src_in);
		let src_file = SourceFile::new(path, src_in);
		
		let result = compiler::compile(&src_file);

		match result.lex.unwrap() {
			Err(err) => {
				println!("Lexer Failed: {}", err.on_red());
				return;
			},
			Ok(tokens) => {
				println!();
				println!("=== Tokens ===");
				for token in tokens.iter() {
					println!("{}", token.with_source(&src_file));
				}
			}
		}

		match result.ast.unwrap() {
			Err(err) => {
				println!("AST Failed: {}", err.on_red());
				println!("{:?}", err);
				return;
			},
			Ok(root) => {
				println!();
				println!("=== AST ===");
				tree_printer::print_tree("Root", &root);
			}
		}

		match result.sat.unwrap() {
			Err(err) => {
				println!("SAT Failed: {}", err.on_red());
				return;
			},
			Ok(root) => {
				println!();
				println!("=== SAT ===");
				println!("\t{}", root);
			}
		}

		match result.gen.unwrap() {
			Err(err) => {
				println!("GEN Failed: {}", err.on_red());
				return;
			},
			Ok(output) => {
				println!();
				println!("=== GEN ===");
				println!("\t{}", output);
			}
		}
	}
}
