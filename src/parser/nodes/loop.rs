use super::Node;

#[derive(Debug)]
pub struct Loop {
	pub body: Box<Node>,
}
