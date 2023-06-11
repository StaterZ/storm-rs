pub use tree_display::TreeDisplay;
use indent::Indent;
use owo_colors::OwoColorize;

mod tree_display;
mod indent;

pub fn print_tree(label: &str, node: &impl TreeDisplay) {
	print_tree_node(label, node, &mut Indent::new());
}

fn print_tree_node(label: &str, node: &(impl TreeDisplay + ?Sized), indent: &mut Indent) {
	println!("{}{}: {}", indent.as_string(), label.green(), node.get_text_line());
	let children = node.get_children();
	if let Some(children) = children {
		indent.stretch();

		if children.len() > 0 {
			for (i, (label, child)) in children.iter().enumerate() {
				indent.push(i + 1 >= children.len());
				print_tree_node(label, *child, indent);
				indent.pop();
			}
		} else {
			indent.push(true);
			println!("{}{}", indent.as_string(), "none".magenta());
			indent.pop();
		}
	}
}