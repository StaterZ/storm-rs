use std::fmt::Display;

use colors_transform::{Color, Hsl};
use owo_colors::{DynColors, OwoColorize};

#[derive(Debug)]
pub struct Var {
	pub name: String,
	pub id: u64,
	pub is_mut: bool, //TODO: BAD IDEA!!! make into proof-based system later
}

impl Var {
	pub fn new(name: String) -> Self {
		Self {
			name,
			id: u64::MAX, //set to unknown state
			is_mut: false, //arbitrary, gets set in SEM stage later
		}
	}
}

impl Display for Var {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.id == u64::MAX {
			write!(f, "{}", self.name)
		} else {
			let golden_angle = 137.50776405003785;
			let h = (self.id as f32 * golden_angle) % 360.0;
			let (r, g, b) = Hsl::from(h, 100.0, 50.0).to_rgb().as_tuple();
			let color = DynColors::Rgb(r as u8, g as u8, b as u8);
			write!(f, "{} {}", self.name, format!("({})", self.id).color(color))
		}
	}
}
