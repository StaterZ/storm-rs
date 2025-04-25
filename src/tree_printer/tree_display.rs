use owo_colors::{AnsiColors, DynColors};

use szu::opt_own::{OptOwn, OptOwnStr};

pub type TreeDisplayChild<'s> = OptOwn<'s, Box<dyn TreeDisplay + 's>, dyn TreeDisplay + 's>;
pub trait TreeDisplay {
	fn get_text_line<'s>(&'s self) -> String;
	fn get_children<'s>(&'s self) -> Option<Vec<(OptOwnStr<'s>, TreeDisplayChild<'s>)>>;

	fn get_scope_color(&self) -> DynColors {
		DynColors::Ansi(AnsiColors::Default)
	}
}
