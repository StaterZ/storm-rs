use super::{Node, Pattern};

#[derive(Debug)]
pub struct Let {
	pub pat: Box<Node<Pattern>>,
}
