mod compiler;
mod tree_printer;

use owo_colors::OwoColorize;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author = "StaterZ")]
struct Args {
    #[arg(short = 'i', long = "in")]
	in_path: Option<std::path::PathBuf>,
    #[arg(short = 'o', long = "out")]
	out_path: Option<std::path::PathBuf>,
	#[arg(short = 'd', long = "debug", action = clap::ArgAction::SetTrue)]
	is_debug: bool,
}

fn main() {
	compile();
}

fn compile() {
	let args = Args::parse();

	let path = if args.is_debug {
		std::path::Path::new("data/in.txt")
	} else {
		args.in_path.as_ref().expect("No input file").as_path()
	};

	if let Ok(src_in) = std::fs::read_to_string(path) {
		println!("=== Source ===");
		println!("{}", src_in);
		
		let result = compiler::compile(src_in);

		match result.lex.unwrap() {
			Err(err) => {
				println!("Lexer Failed: {}", err.on_red());
				return;
			},
			Ok(tokens) => {
				println!();
				println!("=== Tokens ===");
				for token in tokens.iter() {
					println!("{:?}", token);
				}
			}
		}

		match result.ast.unwrap() {
			Err(err) => {
				println!("AST Failed: {}", err.on_red());
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
