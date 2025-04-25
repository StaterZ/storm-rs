mod tree_display;
mod indent;

use std::ops::Deref;

use color_print::cprintln;

pub use tree_display::{TreeDisplay, TreeDisplayChild};
use indent::Indent;

pub fn print_tree(label: &str, node: &impl TreeDisplay, line_formatter: impl Fn(&str, &str) -> String) {
	print_tree_node(label, node, &line_formatter, &mut Indent::new());
}

fn print_tree_node(label: &str, node: &dyn TreeDisplay, line_formatter: &impl Fn(&str, &str) -> String, indent: &mut Indent) {
	cprintln!("{}{}", indent, line_formatter(label, node.get_text_line().deref()));
	
	if let Some(children) = node.get_children() {
		indent.extend();

		let children_len = children.len();
		if children_len > 0 {
			let color = node.get_scope_color();
			for (i, (label, child)) in children.into_iter().enumerate() {
				indent.push(color, i + 1 >= children_len);
				print_tree_node(label.deref(), child.deref(), line_formatter, indent);
				indent.pop();
			}
		} else {
			indent.push(node.get_scope_color(), true);
			cprintln!("{}<magenta>none</>", indent.to_string());
			indent.pop();
		}
	}
}
