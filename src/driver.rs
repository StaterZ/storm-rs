use std::path::PathBuf;
use clap::Parser;
use anyhow::Result;

use lalrpop_util::lalrpop_mod;
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

	let ast = grammar::ExprsParser::new()
		.parse(&mut errors, &src_in);
	println!("RESULT: {:?}", ast);
	println!("ERRORS: {:?}", errors);

	Ok(())
}
