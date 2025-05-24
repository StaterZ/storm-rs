pub mod nodes;
pub mod debug;
mod var;
mod symbol_table;
mod rule_error;

use std::rc::Rc;

use nodes::*;

pub use var::Var;
pub use rule_error::{RuleErrorKind, RuleError, RuleResult};
use symbol_table::SymbolTable;
use crate::{shed_errors, tree_printer};

use super::{
	lexer::{Token, TokenKind, TokenKindTag},
	stream::{Stream, StreamErrorExpectErr, StreamExt},
	ResultSH,
};

use debug::rule_observers::Observer as RuleObserver;

pub trait TokStreamIter<'i> = Iterator<Item = &'i Token>;
pub trait TokStreamRF<'i> = for<'a> Fn(&'a &'i Token) -> &'a &'i Token;
pub trait TokStreamMF<'i> = Fn(&'i Token) -> &'i Token;
pub type TokStream<'i,
	I: /*TokStreamIter<'i>*/,
	RF: /*TokStreamRF<'i>*/,
	MF: /*TokStreamMF<'i>*/,
> = Stream<I, RF, MF, &'i Token>;

fn expect_eq<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Option<&'i Token> {
	stream.expect(|t| t.kind == expected)
}

fn expect_eq_err<'a, 'i>(stream: &mut TokStream<'i,
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

fn error<'a, 'i>(stream: &mut TokStream<'i,
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
	table: &'a mut SymbolTable,
	observer: &'a mut O,
	rule: fn(&mut TokStream<'i, I, RF, MF>, &'a mut SymbolTable, &mut O) -> RuleResult
) -> RuleResult
where
	I: TokStreamIter<'i> + Clone,
	RF: TokStreamRF<'i> + Clone,
	MF: TokStreamMF<'i> + Clone,
	O: RuleObserver<'i>,
{
	let signal = observer.pre_rule(stream);
	let result = stream.try_rule_arg_sh(rule, table, observer);
	observer.post_rule(rule_name, signal, &result);
	
	result
}

fn discard_space<'a, 'i>(stream: &mut TokStream<
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

pub fn parse<'a, 'i>(
	tokens: &'i Vec<Token>,
	observer: &'a mut impl RuleObserver<'i>,
) -> Result<Node, RuleError> {
	let mut stream = tokens.iter().stream(
		|t| t,
		|t| t,
	);

	let mut table = SymbolTable::new();
	match try_rule("file", &mut stream, &mut table, observer, parse_file) {
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

fn parse_file<'a, 'i>(stream: &'a mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	let mut stmts = Vec::new();
	loop {
		discard_space(stream);

		if expect_eq(stream, TokenKind::Eof).is_some() {
			return Ok(Ok(Node { kind: NodeKind::Block(Block {
				stmts
			}) }));
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, table, observer, parse_stmt)? {
			stmts.push(stmt);
			continue;
		}

		return Ok(Err(error(stream)));
	};
}

fn parse_stmt<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	let expr = if let Ok(expr) = try_rule("let", stream, table, observer, parse_let)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("return", stream, table, observer, parse_return)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("break", stream, table, observer, parse_break)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("continue", stream, table, observer, parse_continue)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("unreachable", stream, table, observer, parse_unreachable)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("assign", stream, table, observer, parse_assign)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("expr", stream, table, observer, parse_expr)? {
		Some(expr)
	} else {
		None
	};

	match expr {
		Some(expr) => {
			discard_space(stream);
			match expect_eq(stream, TokenKind::Semicolon) {
				Some(_) => Ok(Ok(Node { kind: NodeKind::Stmt(Stmt {
					expr: Box::new(expr),
				}) })),
				None => Ok(Ok(expr)),
			}
		},
		None => Ok(Err(error(stream))),
	}
}

fn parse_let<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Let) {
		return Ok(Err(err));
	}
	
	discard_space(stream);
	let lhs = Box::new(try_rule("lhs:expr", stream, table, observer, parse_expr)??);
	
	discard_space(stream);
	let rhs = if expect_eq(stream, TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(try_rule("rhs:expr", stream, table, observer, parse_expr)??))
	} else {
		None
	};
	
	Ok(Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) }))
}

fn parse_assign<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = Box::new(shed_errors!(try_rule("lhs:expr", stream, table, observer, parse_expr)));
	
	discard_space(stream);
	if let Err(err) = expect_eq_err(stream, TokenKind::Equals) {
		return Ok(Err(err));
	};
	
	discard_space(stream);
	let rhs = Box::new(try_rule("rhs:expr", stream, table, observer, parse_expr)??);
	
	Ok(Ok(Node { kind: NodeKind::Assign(Assign { lhs, rhs }) }))
}

fn parse_block<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::LBrace) {
		return Ok(Err(err));
	}
	
	let mut stmts = Vec::new();
	loop {
		discard_space(stream);

		if expect_eq(stream, TokenKind::RBrace).is_some() {
			return Ok(Ok(Node { kind: NodeKind::Block(Block {
				stmts
			}) }));
		}
		
		if let Ok(stmt) = try_rule("stmt", stream, table, observer, parse_stmt)? {
			stmts.push(stmt);
			continue;
		}

		return Ok(Err(error(stream)));
	};
}

fn parse_return<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Return) {
		return Ok(Err(err));
	}
	
	discard_space(stream);
	let expr = try_rule("expr", stream, table, observer, parse_expr)?;
	Ok(Ok(Node { kind: NodeKind::Return(Return {
		expr: expr.ok().map(|expr| Box::new(expr)),
	}) }))
}

fn parse_break<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Break) {
		return Ok(Err(err));
	}
	
	discard_space(stream);
	let expr = if let Ok(expr) = try_rule("break", stream, table, observer, parse_break)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("continue", stream, table, observer, parse_continue)? {
		Some(expr)
	} else if let Ok(expr) = try_rule("expr", stream, table, observer, parse_expr)? {
		Some(expr)
	} else {
		None
	};
	
	Ok(Ok(Node { kind: NodeKind::Break(Break {
		expr: expr.map(|expr| Box::new(expr)),
	}) }))
}

fn parse_continue<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, _table: &'a mut SymbolTable, _observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Continue) {
		return Ok(Err(err));
	}
	
	Ok(Ok(Node { kind: NodeKind::Continue }))
}

fn parse_unreachable<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, _table: &'a mut SymbolTable, _observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Unreachable) {
		return Ok(Err(err));
	}

	Ok(Ok(Node { kind: NodeKind::Unreachable }))
}

fn parse_if_else<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::If) {
		return Ok(Err(err));
	}

	discard_space(stream);
	let cond = try_rule("cond:expr", stream, table, observer, parse_expr)??;

	discard_space(stream);
	let body = match expect_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, table, observer, parse_stmt)??
		},
		None => try_rule("body:expr", stream, table, observer, parse_block)??,
	};

	discard_space(stream);
	let body_else = if expect_eq(stream, TokenKind::Else).is_some() {
		discard_space(stream);
		Some(try_rule("else:expr", stream, table, observer, parse_expr)??)
	} else { None };

	Ok(Ok(Node { kind: NodeKind::IfElse(IfElse {
		cond: Box::new(cond),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) }))
}

fn parse_loop<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::Loop) {
		return Ok(Err(err));
	}

	discard_space(stream);
	let body = try_rule("body:expr", stream, table, observer, parse_expr)??;
	
	discard_space(stream);
	let body_else = if expect_eq(stream, TokenKind::Else).is_some() {
		discard_space(stream);
		Some(try_rule("else:expr", stream, table, observer, parse_expr)??)
	} else { None };

	Ok(Ok(Node { kind: NodeKind::Loop(Loop {
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) }))
}

fn parse_while<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::While) {
		return Ok(Err(err));
	}

	discard_space(stream);
	let cond = try_rule("cond:expr", stream, table, observer, parse_expr)??;
	
	discard_space(stream);
	let body = match expect_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, table, observer, parse_expr)??
		},
		None => try_rule("body:expr", stream, table, observer, parse_expr)??,
	};

	discard_space(stream);
	let body_else = if expect_eq(stream, TokenKind::Else).is_some() {
		discard_space(stream);
		Some(try_rule("else:expr", stream, table, observer, parse_expr)??)
	} else { None };

	Ok(Ok(Node { kind: NodeKind::While(While {
		cond: Box::new(cond),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) }))
}

fn parse_for<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::For) {
		return Ok(Err(err));
	}

	discard_space(stream);
	let binding = try_rule("binding:expr", stream, table, observer, parse_expr)??;

	discard_space(stream);
	expect_eq_err(stream, TokenKind::In)?;

	discard_space(stream);
	let iter = try_rule("iter:expr", stream, table, observer, parse_expr)??;
	
	discard_space(stream);
	let body = match expect_eq(stream, TokenKind::Colon) {
		Some(_) => {
			discard_space(stream);
			try_rule("body:expr", stream, table, observer, parse_expr)??
		},
		None => try_rule("body:expr", stream, table, observer, parse_expr)??,
	};

	discard_space(stream);
	let body_else = if expect_eq(stream, TokenKind::Else).is_some() {
		discard_space(stream);
		Some(try_rule("else:expr", stream, table, observer, parse_expr)??)
	} else { None };

	Ok(Ok(Node { kind: NodeKind::For(For {
		binding: Box::new(binding),
		iter: Box::new(iter),
		body: Box::new(body),
		body_else: body_else.map(|body_else| Box::new(body_else)),
	}) }))
}

fn parse_expr<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(expr) = try_rule("block", stream, table, observer, parse_block)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("if-else", stream, table, observer, parse_if_else)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("loop", stream, table, observer, parse_loop)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("while", stream, table, observer, parse_while)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("for", stream, table, observer, parse_for)? {
		return Ok(Ok(expr));
	}
	if let Ok(expr) = try_rule("expr_bin", stream, table, observer, parse_expr_bin)? {
		return Ok(Ok(expr));
	}

	Ok(Err(match stream.get_peeker().get() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}))
}

fn parse_expr_bin<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &mut impl RuleObserver<'i>) -> RuleResult {
	let lhs = shed_errors!(try_rule("atom", stream, table, observer, parse_expr_atom));
	
	let mut stream = stream.dup();
	discard_space(stream.get());
	let op = stream.get().expect_map(|&t| match t.kind {
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
	});

	Ok(Ok(match op {
		None => lhs,
		Some((_, op)) => {
			discard_space(stream.get());
			let mut rhs = parse_expr(stream.get(), table, observer)??;
			stream.nip();

			if let NodeKind::BinOp(rhs_bin_op) = &rhs.kind {
				if rhs_bin_op.op.precedence() > op.precedence() {
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

fn parse_expr_atom<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	if let Ok(tuple_ctor) = try_rule("tuple_ctor", stream, table, observer, parse_tuple_ctor)? {
		return Ok(Ok(tuple_ctor));
	}
	if let Ok(paren) = try_rule("parenthesis", stream, table, observer, parse_parenthesis)? {
		return Ok(Ok(paren));
	}

	stream.try_rule_sh(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Ok(Node { kind: NodeKind::IntLit(*value) })),
			TokenKind::StrLit(value) => Ok(Ok(Node { kind: NodeKind::StrLit(value.clone()) })),
			TokenKind::Identifier(value) => {
				let var = Rc::new(Var {
					name: value.clone(),
				});

				Ok(Ok(Node { kind: NodeKind::Identifier(var) }))
			},
			_ => Ok(Err(RuleErrorKind::UnexpectedToken((&token.kind).into()))),
		},
		None => Ok(Err(RuleErrorKind::StreamExhausted)),
	})
}

fn parse_parenthesis<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::LParen) {
		return Ok(Err(err));
	}
	discard_space(stream);


	let expr = try_rule("expr", stream, table, observer, parse_expr)??;
	discard_space(stream);
	
	expect_eq_err(stream, TokenKind::RParen)?;
	discard_space(stream);

	Ok(Ok(expr))
}

fn parse_tuple_ctor<'a, 'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>, table: &'a mut SymbolTable, observer: &'a mut impl RuleObserver<'i>) -> RuleResult {
	if let Err(err) = expect_eq_err(stream, TokenKind::LParen) {
		return Ok(Err(err));
	}
	discard_space(stream);

	let mut tuple_ctor = TupleCtor::new();
	while expect_eq(stream, TokenKind::RParen).is_none() {
		let expr = try_rule("expr:arg", stream, table, observer, parse_expr)??;
		tuple_ctor.items.push(expr);

		discard_space(stream);
		if expect_eq(stream, TokenKind::Comma).is_none() {
			discard_space(stream);
			if let Err(err) = expect_eq_err(stream, TokenKind::RParen) {
				return Err(err);
			}
			if tuple_ctor.items.len() == 1 {
				return Ok(Err(RuleErrorKind::UnexpectedToken(TokenKindTag::RParen)));
			}
			break;
		}
		discard_space(stream);
	}
	discard_space(stream);

	Ok(Ok(Node { kind: NodeKind::TupleCtor(tuple_ctor) }))
}
