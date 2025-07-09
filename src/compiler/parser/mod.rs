pub mod nodes;
pub mod debug;
mod var;
mod rule_error;

use std::rc::Rc;

use nodes::*;

pub use var::Var;
pub use rule_error::{RuleErrorKind, RuleError, RuleResult};
use crate::compiler::{
	map_peekable::{
		MapPeekable, MapPeekableExpectError, PeekableIterator,
		soft_error::{SoftError, SoftResult, SoftResultTrait, SoftResultTraitSame},
	},
	lexer::{Token, TokenKind, TokenKindTag},
};

use debug::rule_observers::Observer as RuleObserver;

pub trait TokStreamIter<'i> = PeekableIterator<Item = &'i Token> + Clone;
pub trait TokStreamRF<'i> = (for<'a> Fn(&'a &'i Token) -> &'a &'i Token) + Clone;
pub trait TokStreamMF<'i> = (Fn(&'i Token) -> &'i Token) + Clone;
pub type TokStream<'i,
	I: /*TokStreamIter<'i>*/,
	RF: /*TokStreamRF<'i>*/,
	MF: /*TokStreamMF<'i>*/,
> = MapPeekable<I, RF, MF, &'i Token>;

fn next_if_eq<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Option<&'i Token> {
	stream.next_if(|t| t.kind == expected)
}

fn next_if_err<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, pred: impl FnOnce(&'i Token) -> bool) -> Result<&'i Token, RuleErrorKind> {
	stream.next_if_err(|t| if pred(t) {
		Ok(())
	} else {
		Err((&t.kind).into())
	}).map_err(|err| match err {
		MapPeekableExpectError::StreamExhausted => RuleErrorKind::StreamExhausted,
		MapPeekableExpectError::PredicateError(found) => RuleErrorKind::UnexpectedToken(found),
	})
}

fn next_if_eq_err<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Result<&'i Token, RuleErrorKind> {
	stream.next_if_err(|t| if t.kind == expected {
		Ok(())
	} else {
		Err((&t.kind).into())
	}).map_err(|err| match err {
		MapPeekableExpectError::StreamExhausted => RuleErrorKind::StreamExhausted,
		MapPeekableExpectError::PredicateError(found) => RuleErrorKind::ExpectedToken {
			expected: (&expected).into(),
			found,
		},
	})
}

fn error<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>) -> RuleErrorKind {
	match stream.peek() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}
}

fn try_rule<'a, 'i, I, RF, MF, O>(
	rule_name: &'static str,
	stream: &'a mut TokStream<'i, I, RF, MF>,
	observer: &'a mut O,
	rule: fn(&mut TokStream<'i, I, RF, MF>, &mut O) -> RuleResult
) -> RuleResult
where
	I: TokStreamIter<'i> + Clone,
	RF: TokStreamRF<'i> + Clone,
	MF: TokStreamMF<'i> + Clone,
	O: RuleObserver<'i>,
{
	let signal = observer.pre_rule(stream);
	let result = stream.try_rule_sh_arg(rule, observer);
	observer.post_rule(rule_name, signal, &result);
	
	result
}

fn discard_space<'a, 'i>(stream: &mut TokStream<
	'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>) {
	while stream.next_if(|t| matches!(t.kind,
		| TokenKind::Space
		| TokenKind::NewLine
		| TokenKind::Comment
		| TokenKind::MultilineComment
	)).is_some() { };
}

pub fn parse<'a, 'i>(
	tokens: &'i Vec<Token>,
	observer: &'a mut impl RuleObserver<'i>,
) -> Result<Node, RuleError> {
	let mut stream = tokens.iter().peekable().map_peekable(
		|t| t,
		|t| t,
	);

	match try_rule("file", &mut stream, observer, parse_file) {
		Ok(file) => Ok(file),
		Err(err) => Err(RuleError {
			kind: err.value(),
			source_range: stream
				.peek()
				.map(|t| t.range),
		}),
	}
}

fn parse_file<'a, 'i>(stream: &'a mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	let mut stmts = Vec::new();
	loop {
		discard_space(stream);

		if next_if_eq(stream, TokenKind::Eof).is_some() {
			return Ok(Node { kind: NodeKind::Block(Block {
				stmts
			}) });
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt).shed_hard()? {
			stmts.push(stmt);
			continue;
		}

		return Err(SoftError::Soft(error(stream)));
	};
}

fn parse_stmt<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	let expr = if let Ok(expr) = try_rule("return", stream, observer, parse_return).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("break", stream, observer, parse_break).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("continue", stream, observer, parse_continue).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("unreachable", stream, observer, parse_unreachable).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("assign", stream, observer, parse_assign).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("expr", stream, observer, parse_expr).shed_hard()? {
		Some(expr)
	} else {
		None
	};

	match expr {
		Some(expr) => {
			discard_space(stream);
			match next_if_eq(stream, TokenKind::Semicolon) {
				Some(_) => Ok(Node { kind: NodeKind::Stmt(Stmt {
					expr: Box::new(expr),
				}) }),
				None => Ok(expr),
			}
		},
		None => Err(SoftError::Soft(error(stream))),
	}
}

fn parse_assign<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = try_rule("lhs:pattern", stream, observer, parse_pattern)?;
	
	discard_space(stream);
	if let Err(err) = next_if_eq_err(stream, TokenKind::Equals) {
		return Err(SoftError::Soft(err));
	};
	
	discard_space(stream);
	let rhs = try_rule("rhs:expr", stream, observer, parse_expr).force_hard()?;
	
	Ok(Node { kind: NodeKind::Assign(Assign {
		lhs: Box::new(lhs),
		rhs: Box::new(rhs),
	}) })
}

fn parse_pattern<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(expr) = try_rule("let", stream, observer, parse_let).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("mut", stream, observer, parse_mut).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("tuple_dtor", stream, observer, parse_tuple_dtor).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("expr", stream, observer, parse_expr).shed_hard()? {
		return Ok(expr);
	}

	Err(SoftError::Soft(match stream.peek() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}))
}

fn parse_let<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Let).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let expr = try_rule("expr", stream, observer, parse_pattern).force_hard()?;
	
	Ok(Node { kind: NodeKind::Let(Let { expr: Box::new(expr) }) })
}

fn parse_mut<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Mut).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let expr = try_rule("expr", stream, observer, parse_pattern).force_hard()?;
	
	Ok(Node { kind: NodeKind::Mut(Mut { expr: Box::new(expr) }) })
}

fn parse_block<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::LBrace).map_err(|err| SoftError::Soft(err))?;
	
	let mut stmts = Vec::new();
	loop {
		discard_space(stream);

		if next_if_eq(stream, TokenKind::RBrace).is_some() {
			return Ok(Node { kind: NodeKind::Block(Block {
				stmts
			}) });
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt).shed_hard()? {
			stmts.push(stmt);
			continue;
		}

		return Err(SoftError::Soft(error(stream)));
	};
}

fn parse_return<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Return).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let expr = try_rule("expr", stream, observer, parse_expr).shed_hard()?;
	Ok(Node { kind: NodeKind::Return(Return {
		expr: expr.ok().map(|expr| Box::new(expr)),
	}) })
}

fn parse_break<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Break).map_err(|err| SoftError::Soft(err))?;
	
	discard_space(stream);
	let expr = if let Ok(expr) = try_rule("break", stream, observer, parse_break).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("continue", stream, observer, parse_continue).shed_hard()? {
		Some(expr)
	} else if let Ok(expr) = try_rule("expr", stream, observer, parse_expr).shed_hard()? {
		Some(expr)
	} else {
		None
	};
	
	Ok(Node { kind: NodeKind::Break(Break {
		expr: expr.map(|expr| Box::new(expr)),
	}) })
}

fn parse_continue<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, _observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Continue).map_err(|err| SoftError::Soft(err))?;
	
	Ok(Node { kind: NodeKind::Continue })
}

fn parse_unreachable<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, _observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Unreachable).map_err(|err| SoftError::Soft(err))?;

	Ok(Node { kind: NodeKind::Unreachable })
}

fn parse_if<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::If).map_err(|err| SoftError::Soft(err))?;
	
	discard_space(stream);
	let cond = try_rule("cond:expr", stream, observer, parse_expr).force_hard()?;

	discard_space(stream);
	let body = match next_if_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, observer, parse_stmt).force_hard()?
		},
		None => try_rule("body:expr", stream, observer, parse_block).force_hard()?,
	};

	discard_space(stream);
	let body_else = next_if_eq(stream, TokenKind::Else).map(|_| {
		discard_space(stream);
		try_rule("else:expr", stream, observer, parse_expr)
	}).transpose().force_hard()?;

	Ok(Node { kind: NodeKind::If(If {
		cond: Box::new(cond),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) })
}

fn parse_loop<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::Loop).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let body = try_rule("body:expr", stream, observer, parse_expr).force_hard()?;
	
	discard_space(stream);
	let body_else = next_if_eq(stream, TokenKind::Else).map(|_| {
		discard_space(stream);
		try_rule("else:expr", stream, observer, parse_expr)
	}).transpose().force_hard()?;

	Ok(Node { kind: NodeKind::Loop(Loop {
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) })
}

fn parse_while<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::While).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let cond = try_rule("cond:expr", stream, observer, parse_expr).force_hard()?;
	
	discard_space(stream);
	let body = match next_if_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, observer, parse_expr).force_hard()?
		},
		None => try_rule("body:expr", stream, observer, parse_expr).force_hard()?,
	};

	discard_space(stream);
	let body_else = next_if_eq(stream, TokenKind::Else).map(|_| {
		discard_space(stream);
		try_rule("else:expr", stream, observer, parse_expr)
	}).transpose().force_hard()?;

	Ok(Node { kind: NodeKind::While(While {
		cond: Box::new(cond),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) })
}

fn parse_for<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::For).map_err(|err| SoftError::Soft(err))?;

	discard_space(stream);
	let binding = try_rule("binding:expr", stream, observer, parse_expr).force_hard()?;

	discard_space(stream);
	next_if_eq_err(stream, TokenKind::In).map_err(|err| SoftError::Hard(err))?;

	discard_space(stream);
	let iter = try_rule("iter:expr", stream, observer, parse_expr).force_hard()?;
	
	discard_space(stream);
	let body = match next_if_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, observer, parse_expr).force_hard()?
		},
		None => try_rule("body:expr", stream, observer, parse_expr).force_hard()?,
	};

	discard_space(stream);
	let body_else = next_if_eq(stream, TokenKind::Else).map(|_| {
		discard_space(stream);
		try_rule("else:expr", stream, observer, parse_expr)
	}).transpose().force_hard()?;

	Ok(Node { kind: NodeKind::For(For {
		binding: Box::new(binding),
		iter: Box::new(iter),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) })
}

fn parse_expr<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(expr) = try_rule("block", stream, observer, parse_block).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("if-else", stream, observer, parse_if).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("loop", stream, observer, parse_loop).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("while", stream, observer, parse_while).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("for", stream, observer, parse_for).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("expr_bin", stream, observer, parse_expr_bin).shed_hard()? {
		return Ok(expr);
	}

	Err(SoftError::Soft(match stream.peek() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}))
}

fn parse_expr_bin<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = try_rule("expr_una", stream, observer, parse_expr_una)?;
	
	let stream_recover_state = stream.clone();
	discard_space(stream);
	let op = stream.next_if_map(|&t| match t.kind {
		TokenKind::Plus => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Add, allow_wrap: false })),
		TokenKind::Dash => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Sub, allow_wrap: false })),
		TokenKind::Star => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Mul, allow_wrap: false })),
		TokenKind::Slash => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Div, allow_wrap: false })),
		TokenKind::Percent => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Mod, allow_wrap: false })),

		TokenKind::LShift => Some(BinOpKind::Bitwise(BitwiseBinOpKind::Shl)),
		TokenKind::RShift => Some(BinOpKind::Bitwise(BitwiseBinOpKind::Shr)),
		
		TokenKind::Eq => Some(BinOpKind::Cmp(CmpBinOpKind::Eq)),
		TokenKind::Ne => Some(BinOpKind::Cmp(CmpBinOpKind::Ne)),
		TokenKind::Lt => Some(BinOpKind::Cmp(CmpBinOpKind::Lt)),
		TokenKind::Le => Some(BinOpKind::Cmp(CmpBinOpKind::Le)),
		TokenKind::Gt => Some(BinOpKind::Cmp(CmpBinOpKind::Gt)),
		TokenKind::Ge => Some(BinOpKind::Cmp(CmpBinOpKind::Ge)),

		TokenKind::And => Some(BinOpKind::Logic(LogicBinOpKind::And)),
		TokenKind::Or => Some(BinOpKind::Logic(LogicBinOpKind::Or)),

		//TokenKind::Equals => Some(BinOpKind::Assign(None)),
		_ => None,
	}).map(|(_, op)| op);

	match op {
		None => {
			*stream = stream_recover_state;
			Ok(lhs)
		},
		Some(op) => {
			discard_space(stream);
			let mut rhs = parse_expr(stream, observer).force_hard()?;

			if let NodeKind::BinOp(rhs_bin_op) = &rhs.kind {
				if rhs_bin_op.op.precedence() > op.precedence() {
					let mut rhs_bin_op = rhs.kind.into_bin_op().unwrap(); //safe due to if-let above.

					rhs_bin_op.lhs = Box::new(Node{
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: rhs_bin_op.lhs,
						}),
					});
					rhs.kind = NodeKind::BinOp(rhs_bin_op);
					Ok(rhs)
				} else {
					Ok(Node {
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: Box::new(rhs),
						}
					)})
				}
			} else {
				Ok(Node {
					kind: NodeKind::BinOp(BinOp {
						op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					}
				)})
			}
		},
	}
}

fn parse_expr_una<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let expr = try_rule("expr_atom", stream, observer, parse_expr_atom)?;

	discard_space(stream);
	let stream_recover_state = stream.clone();
	let Some(token) = stream.next() else {
		return Ok(expr);
	};

	Ok(match &token.kind {
		TokenKind::Star => Node { kind: NodeKind::Deref(Box::new(expr)) },
		TokenKind::Ampersand => Node { kind: NodeKind::AddressOf(Box::new(expr)) },
		TokenKind::Dot => {
			discard_space(stream);
			let ident = parse_identifier(stream).force_hard()?;
			Node { kind: NodeKind::FieldAccess(FieldAccess { ident, expr: Box::new(expr) }) }
		},
		_ => {
			*stream = stream_recover_state;
			expr
		},
	})
}

fn parse_expr_atom<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	match try_rule("tuple_ctor", stream, observer, parse_tuple_ctor) {
		Ok(value) => return Ok(value),
		Err(SoftError::Hard(err)) => return Err(SoftError::Hard(err)),
		Err(SoftError::Soft(_err)) => {},
	}
	match try_rule("parenthesis", stream, observer, parse_parenthesis) {
		Ok(value) => return Ok(value),
		Err(SoftError::Hard(err)) => return Err(SoftError::Hard(err)),
		Err(SoftError::Soft(_err)) => {},
	}

	stream.try_rule_sh(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: NodeKind::IntLit(value.clone()) }),
			TokenKind::StrLit(value) => Ok(Node { kind: NodeKind::StrLit(value.clone()) }),
			TokenKind::Identifier(value) => {
				let var = Rc::new(Var {
					name: value.clone(),
				});

				Ok(Node { kind: NodeKind::Identifier(var) })
			},
			_ => Err(SoftError::Soft(RuleErrorKind::UnexpectedToken((&token.kind).into()))),
		},
		None => Err(SoftError::Soft(RuleErrorKind::StreamExhausted)),
	})
}

fn parse_identifier<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>) -> SoftResult<Rc<Var>, RuleErrorKind, RuleErrorKind> {
	let ident = next_if_err(stream, |t| matches!(t.kind, TokenKind::Identifier(_)))
		.map_err(|err| SoftError::Soft(err))?;

	Ok(Rc::new(Var {
		name: ident.kind.as_identifier().unwrap().clone(), //unwarp safe due to guard above
	}))
}

fn parse_parenthesis<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::LParen).map_err(|err| SoftError::Soft(err))?;
	discard_space(stream);

	let expr = try_rule("expr", stream, observer, parse_expr).force_hard()?;
	discard_space(stream);
	
	next_if_eq_err(stream, TokenKind::RParen)
		.map_err(|err| SoftError::Hard(err))?;
	discard_space(stream);

	Ok(expr)
}

fn parse_tuple_ctor<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::LParen).map_err(|err| SoftError::Soft(err))?;
	discard_space(stream);

	let mut items = Vec::new();
	while next_if_eq(stream, TokenKind::RParen).is_none() {
		let expr = try_rule("expr:arg", stream, observer, parse_expr).force_hard()?;
		items.push(expr);

		discard_space(stream);
		if next_if_eq(stream, TokenKind::Comma).is_none() {
			discard_space(stream);
			if let Err(err) = next_if_eq_err(stream, TokenKind::RParen) {
				return Err(SoftError::Hard(err));
			}
			if items.len() == 1 {
				return Err(SoftError::Soft(RuleErrorKind::UnexpectedToken(TokenKindTag::RParen)));
			}
			break;
		}
		discard_space(stream);
	}
	discard_space(stream);

	Ok(Node { kind: NodeKind::TupleCtor(TupleCtor { items }) })
}

fn parse_tuple_dtor<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	next_if_eq_err(stream, TokenKind::LParen).map_err(|err| SoftError::Soft(err))?;
	discard_space(stream);

	let mut items = Vec::new();
	while next_if_eq(stream, TokenKind::RParen).is_none() {
		let expr = try_rule("pattern:arg", stream, observer, parse_pattern).force_hard()?;
		items.push(expr);

		discard_space(stream);
		if next_if_eq(stream, TokenKind::Comma).is_none() {
			discard_space(stream);
			if let Err(err) = next_if_eq_err(stream, TokenKind::RParen) {
				return Err(SoftError::Hard(err));
			}
			if items.len() == 1 {
				return Err(SoftError::Soft(RuleErrorKind::UnexpectedToken(TokenKindTag::RParen)));
			}
			break;
		}
		discard_space(stream);
	}
	discard_space(stream);

	Ok(Node { kind: NodeKind::TupleDtor(TupleDtor { items }) })
}
