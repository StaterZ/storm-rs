use super::sat;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	TodoErr,
}

pub fn generate(_sat_root: &sat::Node) -> Result<String, GenError> {
	Ok("TODO".to_string())
}
