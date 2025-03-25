use std::path::PathBuf;
use clap::Parser;
use anyhow::Result;

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

fn build(args: Args) -> Result<()> {
	//let Ok(src_in) = std::fs::read_to_string(&args.in_path)?;

	Ok(())
}
