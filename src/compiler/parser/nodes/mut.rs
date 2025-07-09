use super::Node;

#[derive(Debug)]
pub struct Mut {
	pub expr: Box<Node>,
}
