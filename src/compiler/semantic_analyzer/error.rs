use std::rc::Rc;

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
	#[error("No variable '{0}' is defined")]
	UndefinedSymbol(Rc<Var>),
}
