use super::semantic_analyzer as sem;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	TodoErr,
}

pub fn generate(_sat_root: &sem::Node) -> Result<String, GenError> {
	Ok("TODO".to_string())
}
