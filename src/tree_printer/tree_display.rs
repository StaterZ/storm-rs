use std::ops::Deref;

use owo_colors::{AnsiColors, DynColors};

pub type TreeDisplayChild<'s> = OptOwn<'s, dyn TreeDisplay + 's>;

pub enum OptOwn<'s, T: ?Sized> {
	Ref(&'s T),
	Owned(Box<T>),
}

impl<'s, T: ?Sized> Deref for OptOwn<'s, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		match self {
			Self::Ref(r) => r,
			Self::Owned(o) => o.deref(),
		}
	}
}

pub trait TreeDisplay {
	fn get_text_line(&self) -> String;
	fn get_children<'s>(&'s self) -> Option<Vec<(String, TreeDisplayChild<'s>)>>;

	fn get_scope_color(&self) -> DynColors {
		DynColors::Ansi(AnsiColors::Default)
	}
}
