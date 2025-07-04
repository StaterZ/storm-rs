use std::fmt::Display;

#[derive(Debug)]
pub struct Var {
	pub name: String,
}

impl Display for Var {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.name)
	}
}
