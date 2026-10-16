use std::{cell::RefCell, rc::Rc};

use lalrpop_util::ParseError;

use crate::compiler::{lexer::Token, source::{self, Pos, Sourced}};

use super::{node_sets::*, nodes::*, RuleError, RuleErrorKind, Var};

/// An expression that may still turn out to be a pattern.
/// Assignment targets look like expressions until the `=` shows up, so the grammar parses both as covers and converts them afterwards.
pub enum Cover {
	Expr(Expr),
	Paren(Box<Sourced<Cover>>),
	Tuple(Vec<Sourced<Cover>>),
	Let(Box<Sourced<Cover>>),
	Mut(Box<Sourced<Cover>>),
	Discard,
}

pub type GrammarError = ParseError<usize, Token, RuleError>;

pub fn range(l: usize, r: usize) -> source::Range {
	source::Range {
		begin: Pos::new_todo_remove_me(l),
		end: Pos::new_todo_remove_me(r),
	}
}

pub fn node<T>(l: usize, value: T, r: usize) -> Sourced<T> {
	Sourced::new(value, range(l, r))
}

pub fn cover(l: usize, expr: Expr, r: usize) -> Sourced<Cover> {
	node(l, Cover::Expr(expr), r)
}

pub fn lift(expr: Sourced<Expr>) -> Sourced<Cover> {
	let range = *expr.source();
	Sourced::new(Cover::Expr(expr.inner()), range)
}

pub fn var(name: String) -> Rc<RefCell<Var>> {
	Rc::new(RefCell::new(Var::new(name)))
}

fn error(kind: RuleErrorKind, range: source::Range) -> GrammarError {
	ParseError::User {
		error: RuleError {
			kind,
			source_range: Some(range),
		},
	}
}

pub fn into_expr(cover: Sourced<Cover>) -> Result<Sourced<Expr>, GrammarError> {
	let range = *cover.source();
	let expr = match cover.inner() {
		Cover::Expr(expr) => expr,
		Cover::Paren(inner) => return into_expr(*inner),
		Cover::Tuple(items) => Expr::TupleCtor(TupleCtor {
			items: items.into_iter().map(into_expr).collect::<Result<_, _>>()?,
		}),
		Cover::Let(_) | Cover::Mut(_) | Cover::Discard => return Err(error(RuleErrorKind::PatternInExpr, range)),
	};
	Ok(Sourced::new(expr, range))
}

pub fn into_pattern(cover: Sourced<Cover>) -> Result<Sourced<Pattern>, GrammarError> {
	let range = *cover.source();
	let pattern = match cover.inner() {
		Cover::Expr(Expr::Identifier(var)) => Pattern::Binding(var),
		Cover::Expr(Expr::UnaOp(UnaOp { op: UnaOpKind::Deref, expr })) => Pattern::Deref(expr),
		Cover::Expr(_) => return Err(error(RuleErrorKind::ExprInPattern, range)),
		Cover::Paren(inner) => Pattern::TupleDtor(TupleDtor {
			items: vec![into_pattern(*inner)?],
		}),
		Cover::Tuple(items) => Pattern::TupleDtor(TupleDtor {
			items: items.into_iter().map(into_pattern).collect::<Result<_, _>>()?,
		}),
		Cover::Let(pat) => Pattern::Let(Let { pat: Box::new(into_pattern(*pat)?) }),
		Cover::Mut(pat) => Pattern::Mut(Mut { pat: Box::new(into_pattern(*pat)?) }),
		Cover::Discard => Pattern::Discard,
	};
	Ok(Sourced::new(pattern, range))
}

pub fn bin_op(l: usize, lhs: Sourced<Cover>, op: BinOpKind, rhs: Sourced<Cover>, r: usize) -> Result<Sourced<Cover>, GrammarError> {
	Ok(cover(l, Expr::BinOp(BinOp {
		op,
		lhs: Box::new(into_expr(lhs)?),
		rhs: Box::new(into_expr(rhs)?),
	}), r))
}

pub fn una_op(l: usize, op: UnaOpKind, expr: Sourced<Cover>, r: usize) -> Result<Sourced<Cover>, GrammarError> {
	Ok(cover(l, Expr::UnaOp(UnaOp {
		op,
		expr: Box::new(into_expr(expr)?),
	}), r))
}

pub fn assign(l: usize, lhs: Sourced<Cover>, op: Option<BinOpKind>, rhs: Sourced<Expr>, r: usize) -> Result<Sourced<Expr>, GrammarError> {
	Ok(node(l, Expr::Assign(Assign {
		op,
		lhs: Box::new(into_pattern(lhs)?),
		rhs: Box::new(rhs),
	}), r))
}
