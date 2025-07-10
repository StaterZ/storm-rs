use std::fmt::Display;

use colors_transform::{Color, Hsl};
use owo_colors::{DynColors, OwoColorize};
use rand::{rngs::StdRng, Rng, SeedableRng};

#[derive(Debug)]
pub struct Var {
	pub name: String,
}

impl Display for Var {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let id = std::ptr::addr_of!(*self);
		let seed = id as usize as u64;
		let mut rng = StdRng::seed_from_u64(seed);
		let h = rng.random_range(0.0..360.0);
		let (r, g, b) = Hsl::from(h, 100.0, 50.0).to_rgb().as_tuple();
		let color = DynColors::Rgb(r as u8, g as u8, b as u8);
		write!(f, "{} {}", self.name, format!("({:?})", id).color(color))
	}
}
