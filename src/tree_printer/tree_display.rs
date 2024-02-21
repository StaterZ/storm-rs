use owo_colors::{AnsiColors, DynColors};

pub trait TreeDisplay {
	fn get_text_line(&self) -> String;
	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>>;

	fn get_scope_color(&self) -> DynColors {
		DynColors::Ansi(AnsiColors::Default)
	}
}
