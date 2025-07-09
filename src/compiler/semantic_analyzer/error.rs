use std::{error::Error, fmt::Display};

use crate::compiler::parser;

#[derive(Debug)]
pub enum SemError<'a> {
	MutWithoutLet,
	DoubleMut,
	DoubleLet,
	BadPattern(&'a parser::nodes::Node),
}

impl<'a> Display for SemError<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SemError::MutWithoutLet => write!(f, "Found mut outside let"),
			SemError::DoubleMut => write!(f, "Found mut inside mut"),
			SemError::DoubleLet => write!(f, "Found let inside let"),
			SemError::BadPattern(node) => write!(f, "Malformed pattern {}", node.kind.as_ref()),
		}
	}
}

impl<'a> Error for SemError<'a> { }
