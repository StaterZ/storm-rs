use tree_printer::TreeDisplay;

use super::super::node_sets::*;

#[derive(Debug)]
pub struct Plex {
	pub name: String,
	pub fields: Vec<Field>,
}

#[derive(Debug)]
pub struct Field {
	pub name: String,
	pub r#type: Node<Expr>,
}

impl TreeDisplay for Field {
	fn get_text_line(&self) -> String {
		"".into()
	}

	fn get_children<'s>(&'s self) -> Option<Vec<(szu::opt_own::OptOwnStr<'s>, tree_printer::TreeDisplayChild<'s>)>> {
		Some(vec![
			("name".into(), (&self.name as &dyn TreeDisplay).into()),
			("type".into(), (&self.r#type as &dyn TreeDisplay).into()),
		])
	}
}
