use crate::compiler::parser;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	TodoErr,
}

pub fn generate(_sat_root: &parser::nodes::Node<parser::nodes::Expr>) -> Result<String, GenError> {
	Ok("TODO".to_string())
}
