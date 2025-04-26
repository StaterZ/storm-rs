use std::fmt::Display;

use color_print::cformat;
use szu::opt_own::OptOwnStr;
use crate::tree_printer::{TreeDisplay, TreeDisplayChild};

impl<T: Display> TreeDisplay for T {
	fn get_text_line(&self) -> String {
		cformat!("<cyan>{}</>", self)
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>> {
		None
	}
}

/*
impl<T: TreeDisplay> TreeDisplay for Option<T> {
	fn get_text_line(&self) -> String {
		self.map_or(cformat!("<magenta>none</>"), |some| some.get_text_line())
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(String, TreeDisplayChild<'s>)>> {
		self.and_then(|some| some.get_children())
	}
}

impl<T: TreeDisplay> TreeDisplay for Box<T> {
	fn get_text_line(&self) -> String {
		self.deref().get_text_line()
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		self.deref().get_children()
	}
}
*/
