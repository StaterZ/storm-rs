pub use result_soft_hard_kind::ResultSHKind;
pub use rule_error_kind::RuleErrorKind;
pub use rule_error::RuleError;
pub use rule_error_meta::RuleErrorMeta;

use super::{
	Node,
	ResultSH,
};

mod result_soft_hard_kind;
mod rule_error_kind;
mod rule_error;
mod rule_error_meta;

pub type RuleResult = ResultSH<Node, RuleErrorKind>;

#[macro_export]
macro_rules! shed_errors {
	($expr:expr) => {
		match $expr {
			Ok(Ok(ok)) => ok,
			Ok(Err(err)) => return Ok(Err(err)),
			Err(err) => return Err(err),
		}
	};
}
