use crate::tree_printer::TreeDisplay;
use color_print::cformat;

impl<T: std::fmt::Display> TreeDisplay for T {
	fn get_text_line(&self) -> String {
		cformat!("<cyan>{}</>", self)
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		None
	}
}

/*impl<T: TreeDisplay> TreeDisplay for Option<T> {
	fn get_text_line(&self) -> String {
		self.map_or("none".magenta(), |some| some.get_text_line())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		self.map(|some| some.get_children())
	}
}

impl<T: TreeDisplay> TreeDisplay for Box<T> {
	fn get_text_line(&self) -> String {
		self.deref().get_text_line()
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		self.deref().get_children()
	}
}*/
