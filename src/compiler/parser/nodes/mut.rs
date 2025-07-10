use super::{Node, Pattern};

#[derive(Debug)]
pub struct Mut {
	pub pat: Box<Node<Pattern>>,
}
