use std::{path::Path, process::Command};

use crate::{compiler::{parser::node_sets::*, source::Sourced}, Backend};

mod llvm;
mod lua;
mod stormworks;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	LLVM(llvm::GenError),
	Lua(lua::GenError),
	Stormworks(stormworks::GenError),
}

/// The command that runs what a backend built into `out_path`. `None` when the backend's output can't be run here.
pub fn run_command(backend: Backend, out_path: &Path) -> Option<Command> {
	match backend {
		Backend::LLVM => Some(Command::new(llvm::executable_path())),
		Backend::Lua => {
			let mut command = Command::new("lua");
			command.arg(out_path);
			Some(command)
		},
		Backend::Stormworks => None,
	}
}

pub fn generate(ast: &Sourced<Expr>, backend: Backend, emit_optimized_ir: bool) -> Result<Vec<u8>, GenError> {
	match backend {
		Backend::LLVM => llvm::generate(ast, emit_optimized_ir).map_err(|err| GenError::LLVM(err)),
		Backend::Lua => lua::generate(ast).map_err(|err| GenError::Lua(err)),
		Backend::Stormworks => stormworks::generate(ast).map_err(|err| GenError::Stormworks(err)),
	}
}
