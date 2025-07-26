use crate::compiler::parser::node_sets::*;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	Todo
}

pub fn generate(_ast: &Node<Expr>) -> Result<Vec<u8>, GenError> {
	Err(GenError::Todo)
}
