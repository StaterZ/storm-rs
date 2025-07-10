use super::{Node, Pattern};

#[derive(Debug)]
pub struct TupleDtor {
	pub items: Vec<Node<Pattern>>,
}
