use crate::{compiler::parser::node_sets::*, Backend};

mod llvm;
mod lua;
mod stormworks;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	LLVM(llvm::GenError),
	Lua(lua::GenError),
	Stormworks(stormworks::GenError),
}

pub fn generate(ast: &Node<Expr>, backend: Backend) -> Result<Vec<u8>, GenError> {
	match backend {
		Backend::LLVM => llvm::generate(ast).map_err(|err| GenError::LLVM(err)),
		Backend::Lua => lua::generate(ast).map_err(|err| GenError::Lua(err)),
		Backend::Stormworks => stormworks::generate(ast).map_err(|err| GenError::Stormworks(err)),
	}
}
