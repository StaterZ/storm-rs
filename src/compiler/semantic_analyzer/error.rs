use std::{cell::RefCell, rc::Rc};

use thiserror::Error;

use crate::compiler::parser::Var;

#[derive(Debug, Error)]
pub enum SemError {
	#[error("Found mut outside let")]
	MutWithoutLet,
	#[error("Found mut inside mut")]
	DoubleMut,
	#[error("Found let inside let")]
	DoubleLet,
	#[error("Symbol '{a}' isn't defined", a = .0.borrow())]
	UndefinedSymbol(Rc<RefCell<Var>>),
	#[error("Symbol '{a}' assigned but isn't mutable", a = .0.borrow())]
	AssignImmutableSymbol(Rc<RefCell<Var>>),
}
