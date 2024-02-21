use std::{fmt::Display, error::Error};

use super::super::super::source;
use super::RuleError;

#[derive(Debug)]
pub struct RuleErrorMeta<'a> {
	pub(in super::super) error: RuleError,
	pub(in super::super) document: &'a source::Document,
}

impl<'a> Display for RuleErrorMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "{}", self.error.kind)?;
		if let Some(source_range) = self.error.source_range {
			write!(f, "{}", source::error_gen::generate_error_line(source_range.to_meta(&self.document)))?;
		} else {
			write!(f, "No source location >:/")?;
		}
		Ok(())
	}
}

impl<'a> Error for RuleErrorMeta<'a> { }
