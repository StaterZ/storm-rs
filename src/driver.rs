use std::path::PathBuf;
use clap::Parser;
use anyhow::Result;

use color_print::cformat;
use lalrpop_util::lalrpop_mod;
use stopwatch::Stopwatch;

use crate::tree_printer::print_tree;

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

pub fn main() -> Result<()> {
	let args = Args::parse();

	let mut timer = Stopwatch::start_new();
	let _ = build(args)?;
	timer.stop();

	println!("compile time: {}ms", timer.elapsed().as_millis());
	Ok(())
}

lalrpop_mod!(grammar);

fn build(args: Args) -> Result<()> {
	let src_in = std::fs::read_to_string(&args.in_path)?;
	let mut errors = Vec::new();

	let ast = grammar::TopParser::new()
		.parse(&mut errors, &src_in);
	
	println!("ERRORS");
	for (i, err) in errors.iter().enumerate() {
		println!("{} | {:?}", i, err);
	}
	println!("=== AST ===");
	print_tree("Root", &ast.unwrap(), |label, value| cformat!("<green>{}</>: {}", label, value));
	println!();
	
	Ok(())
}
