mod node;

pub use node::Node;
use super::parser;

pub fn parse<'a>(_ast_root: &parser::nodes::Node) -> Result<Node, ()> {
	Ok(Node::Todo)
}
