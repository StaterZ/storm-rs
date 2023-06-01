pub trait TreeDisplay {
	fn get_text_line(&self) -> String;
	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>>;
}
