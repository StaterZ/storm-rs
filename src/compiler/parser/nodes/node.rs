use crate::compiler::source;

#[derive(Debug)]
pub struct Node<T> {
	pub kind: T,
	pub range: source::Range,
}
