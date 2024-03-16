pub mod nodes;
pub mod debug;
mod rule_error;

use nodes::{
	NodeKind,
	Node,
	Block,
	Give,
	Let,
	BinOp,
	MathBinOpKind,
	MathBinOpVariant,
	BinOpKind,
	CmpBinOpKind,
};

pub use rule_error::{RuleErrorKind, RuleError, RuleResult};
use crate::{shed_errors, tree_printer};

use self::nodes::Paren;

use super::{
	lexer::{Token, TokenKind},
	stream::{Stream, StreamErrorExpectErr, StreamExt},
	ResultSH,
};

use debug::rule_observers::RuleObserver;

pub trait TokStreamIter<'i> = Iterator<Item = &'i Token>;
pub trait TokStreamRF<'i> = for<'a> Fn(&'a &'i Token) -> &'a &'i Token;
pub trait TokStreamMF<'i> = Fn(&'i Token) -> &'i Token;
pub type TokStream<'i,
	I: /*TokStreamIter<'i>*/,
	RF: /*TokStreamRF<'i>*/,
	MF: /*TokStreamMF<'i>*/,
> = Stream<I, RF, MF, &'i Token>;

fn expect_eq<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Option<&'i Token> {
	stream.expect(|t| t.kind == expected)
}

fn expect_eq_err<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Result<&'i Token, RuleErrorKind> {
	stream.expect_err(|t| if t.kind == expected {
		Ok(())
	} else {
		Err((&t.kind).into())
	}).map_err(|err| match err {
		StreamErrorExpectErr::StreamExhausted => RuleErrorKind::StreamExhausted,
		StreamErrorExpectErr::PredicateError(found) => RuleErrorKind::ExpectedToken {
			expected: (&expected).into(),
			found,
		},
	})
}

fn error<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>) -> RuleErrorKind {
	match stream.get_peeker().get() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}
}

fn try_rule<'a, 'i, I, RF, MF, O>(
	rule_name: &'static str,
	stream: &'a mut TokStream<'i, I, RF, MF>,
	observer: &'a mut O,
	rule: fn(&mut TokStream<'i, I, RF, MF>, observer: &mut O) -> RuleResult
) -> RuleResult
where
	I: TokStreamIter<'i> + Clone,
	RF: TokStreamRF<'i> + Clone,
	MF: TokStreamMF<'i> + Clone,
	O: RuleObserver<'i>,
{
	let signal = observer.pre_rule(stream);
	let result = stream.try_rule_arg_sh(rule, observer);
	observer.post_rule(rule_name, signal, &result);
	
	result
}

fn discard_space<'i>(stream: &mut TokStream<
	'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>) {
	while stream.expect(|t| matches!(t.kind,
		| TokenKind::Space
		| TokenKind::NewLine
		| TokenKind::Comment
		| TokenKind::MultilineComment
	)).is_some() { };
}

pub fn parse_ast<'i>(tokens: &'i Vec<Token>, observer: &mut impl RuleObserver<'i>) -> Result<Node, RuleError> {
	let mut stream = tokens.iter().stream(
		|t| t,
		|t| t,
	);

	match try_rule("file", &mut stream, observer, parse_file) {
		Ok(Ok(file)) => Ok(file),
		Ok(Err(err)) | Err(err) => Err(RuleError {
			kind: err,
			source_range: stream
				.get_peeker()
				.get()
				.map(|t| t.range),
		}),
	}
}

fn parse_file<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if expect_eq(stream, TokenKind::Eof).is_some() {
			return Ok(Ok(Node { kind: NodeKind::Block(block) }));
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt)? {
			block.stmts.push(stmt);
			continue;
		}

		return Ok(Err(error(stream)));
	};
}

fn parse_stmt<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let stmt = if let Ok(stmt) = try_rule("let", stream, observer, parse_let)? {
		Some(stmt)
	} else if let Ok(stmt) = try_rule("give", stream, observer, parse_give)? {
		Some(stmt)
	} else if let Ok(stmt) = try_rule("assign", stream, observer, parse_assignment)? {
		Some(stmt)
	} else if let Ok(stmt) = try_rule("expr", stream, observer, parse_expr)? {
		Some(stmt)
	} else {
		None
	};

	if let Some(stmt) = stmt {
		discard_space(stream);
		expect_eq_err(stream, TokenKind::Semicolon)?;
		Ok(Ok(stmt))
	} else {
		Ok(Err(error(stream)))
	}
}

fn parse_let<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Let) {
		return Ok(Err(err));
	}
	
	discard_space(stream);
	let lhs = Box::new(try_rule("LHS expr", stream, observer, parse_expr)??);
	
	discard_space(stream);
	let rhs = if expect_eq(stream, TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(try_rule("RHS expr", stream, observer, parse_expr)??))
	} else {
		None
	};
	
	Ok(Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) }))
}

fn parse_assignment<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = Box::new(shed_errors!(try_rule("LHS expr", stream, observer, parse_expr)));
	
	discard_space(stream);
	let rhs = if expect_eq(stream, TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(try_rule("RHS expr", stream, observer, parse_expr)??))
	} else {
		None
	};
	
	Ok(Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })) //TODO
}

fn parse_block<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::LBrace) {
		return Ok(Err(err));
	}
	
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if expect_eq(stream, TokenKind::RBrace).is_some() {
			return Ok(Ok(Node { kind: NodeKind::Block(block) }));
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt)? {
			block.stmts.push(stmt);
			continue;
		}

		return Ok(Err(error(stream)));
	};
}

fn parse_give<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Give) {
		return Ok(Err(err));
	}
	
	discard_space(stream);
	let expr =  try_rule("expr", stream, observer, parse_expr)??;
	return Ok(Ok(Node { kind: NodeKind::Give(Give { expr: Box::new(expr) }) }));
}

fn parse_expr<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(expr) = try_rule("block", stream, observer, parse_block)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("expr_bin", stream, observer, parse_expr_bin)? {
		return Ok(Ok(expr));
	}

	Ok(Err(match stream.get_peeker().get() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}))
}

fn parse_expr_bin<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = shed_errors!(try_rule("atom", stream, observer, parse_expr_atom));
	
	let mut stream = stream.dup();
	discard_space(stream.get());
	let op = stream.get().expect_map(|&t| match t.kind {
		TokenKind::Plus => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Add, allow_wrap: false })),
		TokenKind::Dash => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Sub, allow_wrap: false })),
		TokenKind::Star => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Mul, allow_wrap: false })),
		TokenKind::Slash => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Div, allow_wrap: false })),
		TokenKind::Percent => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Mod, allow_wrap: false })),
		TokenKind::LShift => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Shl, allow_wrap: false })),
		TokenKind::RShift => Some(BinOpKind::Math(MathBinOpVariant { kind: MathBinOpKind::Shr, allow_wrap: false })),
		//TokenKind::Equals => Some(BinOpKind::Assign(None)),
		TokenKind::Equality => Some(BinOpKind::Cmp(CmpBinOpKind { lt: false, eq: true, gt: false })),
		_ => None,
	});

	Ok(Ok(match op {
		None => lhs,
		Some((_, op)) => {
			discard_space(stream.get());
			let mut rhs = parse_expr(stream.get(), observer)??;
			stream.nip();

			if let NodeKind::BinOp(rhs_bin_op) = &rhs.kind {
				if rhs_bin_op.op < op {
					let mut rhs_bin_op = rhs.kind.into_bin_op().unwrap();

					rhs_bin_op.lhs = Box::new(Node{
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: rhs_bin_op.lhs,
						}),
					});
					rhs.kind = NodeKind::BinOp(rhs_bin_op);
					rhs
				} else {
					Node {
						kind: NodeKind::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: Box::new(rhs),
						}
					)}
				}
			} else {
				Node {
					kind: NodeKind::BinOp(BinOp {
						op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					}
				)}
			}
		},
	}))
}

fn parse_expr_atom<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(paren) = parse_parenthesis(stream, observer)? {
		return Ok(Ok(paren));
	}

	stream.try_rule_sh(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Ok(Node { kind: NodeKind::IntLit(*value) })),
			TokenKind::StrLit(value) => Ok(Ok(Node { kind: NodeKind::StrLit(value.clone()) })),
			TokenKind::Identifier(value) => Ok(Ok(Node { kind: NodeKind::Identifier(value.clone()) })),
			_ => Ok(Err(RuleErrorKind::UnexpectedToken((&token.kind).into()))),
		},
		None => Ok(Err(RuleErrorKind::StreamExhausted)),
	})
}

fn parse_parenthesis<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::LParen) {
		return Ok(Err(err));
	}
	discard_space(stream);


	let expr = try_rule("expr", stream, observer, parse_expr)??;
	discard_space(stream);
	
	expect_eq_err(stream, TokenKind::RParen)?;
	discard_space(stream);

	return Ok(Ok(Node { kind: NodeKind::Paren( Paren { expr: Box::new(expr) }) }));
}
