mod node;

pub use node::Node;
use super::ast;

pub fn parse_sat(_ast_root: &ast::nodes::Node) -> Result<Node, ()> {
	Ok(Node::Todo)
}
