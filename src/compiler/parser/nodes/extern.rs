use std::{cell::RefCell, rc::Rc};

use tree_printer::TreeDisplay;

use crate::compiler::source::Sourced;

use super::super::{node_sets::*, Var};

/// `extern name: type`, a symbol provided from outside the program, like `printf`.
#[derive(Debug, Clone, TreeDisplay)]
pub struct Extern {
	#[tree(skip)]
	pub binding: Rc<RefCell<Var>>,
	pub ty: Sourced<TypeExpr>,
}
