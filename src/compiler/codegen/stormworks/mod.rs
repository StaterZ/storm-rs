use crate::compiler::parser::nodes::*;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	Todo
}

pub fn generate(_ast: &Node<Expr>) -> Result<Vec<u8>, GenError> {
	Err(GenError::Todo)
}
