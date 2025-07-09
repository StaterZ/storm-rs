use crate::compiler::parser;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	TodoErr,
}

pub fn generate(_sat_root: &parser::nodes::Node) -> Result<String, GenError> {
	Ok("TODO".to_string())
}
