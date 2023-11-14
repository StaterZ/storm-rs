#![allow(dead_code)]
#![feature(trait_alias)]

use clap::Parser;
use color_print::cprintln;
use owo_colors::OwoColorize;
use std::path::{Path, PathBuf};
use compiler::source::SourceFile;

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
	
	if let Ok(src_in) = std::fs::read_to_string(path.as_path()) {
		let src_file = SourceFile::new(
			path
				.into_os_string()
				.to_string_lossy()
				.to_string(),
			src_in
		);
		println!("=== Source ===");
		println!("{:?}", src_file.get_content());
		println!();

		let result = compiler::compile(&src_file);

		match result.lex.unwrap() {
			Err(err) => {
				cprintln!("<red>Lexer Failed:</>\n{}", err);
				return;
			},
			Ok(tokens) => {
				println!("=== Tokens ===");
				for token in tokens.iter() {
					println!("{}", token.with_meta(&src_file));
				}
				println!();
			}
		}

		match result.ast.unwrap() {
			Err(err) => {
				cprintln!("<red>AST Failed:</>\n{:?}", err);
				return;
			},
			Ok(root) => {
				println!("=== AST ===");
				tree_printer::print_tree("Root", &root);
				println!();
			}
		}

		match result.sat.unwrap() {
			Err(err) => {
				cprintln!("<red>SAT Failed:</>\n{:?}", err);
				return;
			},
			Ok(root) => {
				println!("=== SAT ===");
				println!("\t{}", root);
				println!();
			}
		}

		match result.gen.unwrap() {
			Err(err) => {
				cprintln!("<red>GEN Failed:</>\n{}", err.on_red());
				return;
			},
			Ok(output) => {
				println!("=== GEN ===");
				println!("\t{}", output);
				println!();
			}
		}
	}
}
