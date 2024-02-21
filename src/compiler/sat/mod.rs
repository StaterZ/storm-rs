mod node;

pub use node::Node;
use super::ast;

pub fn parse_sat(_ast_root: &ast::Node) -> Result<Node, ()> {
	Ok(Node::Todo)
}
