use super::Node;

#[derive(Debug)]
pub struct Paren {
	pub expr: Box<Node>
}
