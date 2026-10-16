#![allow(dead_code)]
#![feature(const_trait_impl)]
#![feature(try_blocks)]
#![feature(min_specialization)]

mod compiler;

use std::path::{Path, PathBuf};

use clap::{Args, Parser, Subcommand, ValueEnum};

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
	#[command(subcommand)]
	command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
	Build(BuildArgs),
	Run(BuildArgs),
	Interactive,
}

#[derive(Args, Debug)]
struct BuildArgs {
	#[arg(
		short = 'b',
		long = "backend",
	)]
	#[clap(value_enum)]
	backend: Backend,

	#[arg(
		short = 'i',
		long = "in",
	)]
	in_path: PathBuf,

	#[arg(
		short = 'o',
		long = "out"
	)]
	out_path: Option<PathBuf>,

	/// Print every stage of the compiler, like all the --show- flags together
	#[arg(long = "show-all")]
	show_all: bool,

	#[command(flatten)]
	flags: compiler::Flags,
}

fn main() {
	match Cli::parse().command {
		Command::Build(args) => if build(args).is_none() {
			std::process::exit(1);
		},
		Command::Run(args) => {
			let backend = args.backend;
			let Some(out_path) = build(args) else {
				std::process::exit(1);
			};
			run(backend, &out_path);
		},
		Command::Interactive => compiler::interactive::run(),
	}
}

fn build(args: BuildArgs) -> Option<PathBuf> {
	let backend = args.backend;
	let in_path = args.in_path;
	let out_path = args.out_path.unwrap_or(in_path.with_extension("out"));

	let content = match std::fs::read_to_string(&in_path) {
		Ok(content) => content,
		Err(err) => {
			eprintln!("Failed to read input file: {}", err);
			return None;
		}
	};

	let document = source::Document::new(
			in_path
			.into_os_string()
			.to_string_lossy()
			.to_string(),
		content
	);

	let mut flags = args.flags;
	if args.show_all {
		flags.show_all();
	}

	let mut timer = Stopwatch::start_new();
	let result = compiler::compile(&document, backend, flags);
	timer.stop();
	println!();
	println!("compile time: {}ms", timer.elapsed().as_millis());
	let output = match result {
		Ok(output) => {
			cprintln!("<yellow>HAPPY! ^v^</>");
			output
		},
		Err(_) => {
			cprintln!("<blue>SAD >~<<</>");
			return None;
		},
	};

	if let Err(err) = std::fs::write(&out_path, output) {
		eprintln!("Failed to write output file: {}", err);
		return None;
	}
	Some(out_path)
}

/// Runs a build's output and exits with its exit code.
fn run(backend: Backend, out_path: &Path) {
	let Some(mut command) = compiler::codegen::run_command(backend, out_path) else {
		eprintln!("The {:?} backend's output can't be run here", backend);
		std::process::exit(1);
	};

	println!("Running...");
	let status = match command.status() {
		Ok(status) => status,
		Err(err) => {
			eprintln!("Failed to run {:?}: {}", command.get_program(), err);
			std::process::exit(1);
		},
	};
	let code = status.code().unwrap_or(1);
	println!("Exit code was: {}", code);
	std::process::exit(code);
}
