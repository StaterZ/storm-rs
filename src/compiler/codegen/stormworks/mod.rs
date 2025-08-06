use crate::compiler::{parser::node_sets::*, source::Sourced};

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	Todo
}

pub fn generate(_ast: &Sourced<Expr>) -> Result<Vec<u8>, GenError> {
	Err(GenError::Todo)
}
