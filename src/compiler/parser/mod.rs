pub mod node_sets;
pub mod nodes;
pub mod debug;
mod var;
mod rule_error;

use std::{cell::RefCell, rc::Rc};

use node_sets::*;
use nodes::*;

pub use var::Var;
pub use rule_error::{RuleErrorKind, RuleError, RuleResult, CreateOrPass, RuleResultCreateOrPass};
use crate::compiler::{
	lexer::{Token, TokenKind, TokenKindTag}, map_peekable::{
		soft_error::{SoftError, SoftResult, SoftResultTrait, SoftResultTraitSame},
		PeekIterUtils,
		PeekableIterator,
		PeekableNextIfError
	},
	source::{self, Pos},
};

use debug::rule_observers::Observer as RuleObserver;

pub trait TokStream<'i> = PeekableIterator<Item = &'i Token> + Clone;

fn next_if_eq<'a, 'i>(stream: &mut impl TokStream<'i>, expected: TokenKindTag) -> Option<&'i Token> {
	stream.next_if(|t| Into::<TokenKindTag>::into(&t.kind) == expected)
}

fn next_if_eq_err<'a, 'i>(stream: &mut impl TokStream<'i>, expected: TokenKindTag) -> Result<&'i Token, RuleErrorKind> {
	stream.next_if_err(|t| if Into::<TokenKindTag>::into(&t.kind) == expected {
		Ok(())
	} else {
		Err((&t.kind).into())
	}).map_err(|err| match err {
		PeekableNextIfError::StreamExhausted => RuleErrorKind::StreamExhausted,
		PeekableNextIfError::PredicateError(found) => RuleErrorKind::ExpectedToken { expected, found },
	})
}

fn create_unreachable_error<'a, 'i>(stream: &mut impl TokStream<'i>) -> RuleErrorKind {
	match stream.peek() {
		Some(token) => RuleErrorKind::UnexpectedToken((&token.kind).into()),
		None => RuleErrorKind::StreamExhausted,
	}
}

fn try_rule<'a, 'i, T, I: TokStream<'i>, O: RuleObserver<'i>>(
	rule_name: &'static str,
	stream: &'a mut I,
	observer: &'a mut O,
	rule: fn(&mut I, &mut O) -> RuleResult<T>
) -> RuleResult<T> {
	let signal = observer.pre_rule(stream);
	let result = stream.try_rule_sh_arg(rule, observer);
	observer.post_rule(rule_name, signal, &result);
	
	result
}

fn create_node<'i, T, I: TokStream<'i>>(
	stream: &mut I,
	f: impl FnOnce(&mut I) -> SoftResult<T, RuleErrorKind, RuleErrorKind>
) -> RuleResult<T> {
	let begin = stream.peek().unwrap().range.begin; //TODO: unsafe unwrap!!!
	let kind = f(stream)?;
	let end = stream.peek().map_or(Pos::new_todo(0), |t| t.range.begin); //TODO: unsafe unwrap!!! //NOTE: this should be end of the last node, not begin of the node after the last, so this is a bad approximation. fix me later
	Ok(Node {
		kind,
		range: source::Range {
			begin,
			end,
		},
	})
}

fn create_node_or_pass<'i, T, I: TokStream<'i>>(
	stream: &mut I,
	f: impl FnOnce(&mut I) -> RuleResultCreateOrPass<T>
) -> RuleResult<T> {
	let begin = stream.peek().ok_or(SoftError::Soft(RuleErrorKind::StreamExhausted))?.range.begin;
	let kind = match f(stream)? {
		CreateOrPass::Create(kind) => kind,
		CreateOrPass::Pass(node) => return Ok(node),
	};
	let end = stream.peek().map_or_else(|| todo!(), |t| t.range.begin); //TODO: this should be end of the last node, not begin of the node after the last, so this is a bad approximation. fix me later
	Ok(Node {
		kind,
		range: source::Range {
			begin,
			end,
		},
	})
}

fn is_space(token: &Token) -> bool {
	matches!(token.kind,
		| TokenKind::Space
		| TokenKind::NewLine
		| TokenKind::Comment
		| TokenKind::MultilineComment)
}

pub fn parse<'a, 'i>(
	tokens: &'i Vec<Token>,
	observer: &'a mut impl RuleObserver<'i>,
) -> Result<Node<Expr>, RuleError> {
	let mut stream = tokens
		.iter()
		.filter(|t| !is_space(t))
		.peekable()
		.map_peekable(
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

fn parse_file<'a, 'i>(stream: &'a mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		let mut stmts = Vec::new();
		loop {
			if next_if_eq(stream, TokenKindTag::Eof).is_some() {
				return Ok(Expr::Block(Block { stmts, expr: None }));
			}
			
			if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt).shed_hard()? {
				stmts.push(stmt);
				continue;
			}

			return Err(SoftError::Soft(create_unreachable_error(stream)));
		};
	})
}

fn parse_stmt<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node_or_pass(stream, |stream| {
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
				match next_if_eq(stream, TokenKindTag::Semicolon) {
					Some(_) => Ok(CreateOrPass::Create(Expr::Stmt(Stmt {
						expr: Box::new(expr),
					}))),
					None => Ok(CreateOrPass::Pass(expr)),
				}
			},
			None => Err(SoftError::Soft(create_unreachable_error(stream))),
		}
	})
}

fn parse_assign<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		let lhs = try_rule("lhs:pattern", stream, observer, parse_pattern)?;
		let op = stream.try_rule_opt(parse_bin_op);

		if let Err(err) = next_if_eq_err(stream, TokenKindTag::Equals) {
			return Err(SoftError::Soft(err));
		};
		
		let rhs = try_rule("rhs:expr", stream, observer, parse_expr).force_hard()?;
		
		Ok(Expr::Assign(Assign {
			op,
			lhs: Box::new(lhs),
			rhs: Box::new(rhs),
		}))
	})
}

fn parse_pattern<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	if let Ok(pat) = try_rule("let", stream, observer, parse_let).shed_hard()? {
		return Ok(pat);
	}
	if let Ok(pat) = try_rule("mut", stream, observer, parse_mut).shed_hard()? {
		return Ok(pat);
	}
	if let Ok(pat) = try_rule("tuple_dtor", stream, observer, parse_tuple_dtor).shed_hard()? {
		return Ok(pat);
	}
	if let Ok(pat) = try_rule("binding", stream, observer, parse_binding).shed_hard()? {
		return Ok(pat);
	}
	if let Ok(pat) = try_rule("deref", stream, observer, parse_deref).shed_hard()? {
		return Ok(pat);
	}

	Err(SoftError::Soft(create_unreachable_error(stream)))
}

fn parse_deref<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	create_node(stream, |stream| {
		let expr = try_rule("expr_atom", stream, observer, parse_expr_atom)?;

		next_if_eq_err(stream, TokenKindTag::Hash).map_err(|err| SoftError::Soft(err))?;

		Ok(Pattern::Deref(Box::new(expr)))
	})
}

fn parse_let<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Let).map_err(|err| SoftError::Soft(err))?;

		let pat = try_rule("pattern", stream, observer, parse_pattern).force_hard()?;
		
		Ok(Pattern::Let(Let { pat: Box::new(pat) }))
	})
}

fn parse_mut<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Mut).map_err(|err| SoftError::Soft(err))?;

		let pat = try_rule("pattern", stream, observer, parse_pattern).force_hard()?;
		
		Ok(Pattern::Mut(Mut { pat: Box::new(pat) }))
	})
}

fn parse_block<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::LBrace).map_err(|err| SoftError::Soft(err))?;
		
		let mut stmts = Vec::new();
		loop {

			if next_if_eq(stream, TokenKindTag::RBrace).is_some() {
				return Ok(Expr::Block(Block {
					stmts,
					expr: None,
				}));
			}
			
			if let Ok(stmt) = try_rule("stmt", stream, observer, parse_stmt).shed_hard()? {
				stmts.push(stmt);
				continue;
			}

			return Err(SoftError::Soft(create_unreachable_error(stream)));
		};
	})
}

fn parse_return<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Return).map_err(|err| SoftError::Soft(err))?;

		let expr = try_rule("expr", stream, observer, parse_expr).shed_hard()?;
		Ok(Expr::Return(Return {
			expr: expr.ok().map(|expr| Box::new(expr)),
		}))
	})
}

fn parse_break<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Break).map_err(|err| SoftError::Soft(err))?;
		
		let expr = if let Ok(expr) = try_rule("break", stream, observer, parse_break).shed_hard()? {
			Some(expr)
		} else if let Ok(expr) = try_rule("continue", stream, observer, parse_continue).shed_hard()? {
			Some(expr)
		} else if let Ok(expr) = try_rule("expr", stream, observer, parse_expr).shed_hard()? {
			Some(expr)
		} else {
			None
		};
		
		Ok(Expr::Break(Break {
			expr: expr.map(|expr| Box::new(expr)),
		}))
	})
}

fn parse_continue<'a, 'i>(stream: &mut impl TokStream<'i>, _observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Continue).map_err(|err| SoftError::Soft(err))?;
		Ok(Expr::Continue)
	})
}

fn parse_unreachable<'a, 'i>(stream: &mut impl TokStream<'i>, _observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Unreachable).map_err(|err| SoftError::Soft(err))?;
		Ok(Expr::Unreachable)
	})
}

fn parse_plex<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Plex).map_err(|err| SoftError::Soft(err))?;
		//let name = next_if_eq_err(stream, TokenKindTag::Identifier(_)).map_err(|err| SoftError::Hard(err))?.kind.into_identifier().unwrap();
		next_if_eq_err(stream, TokenKindTag::LBrace).map_err(|err| SoftError::Hard(err))?;
		
		let mut fields = Vec::new();
		loop {

			if next_if_eq(stream, TokenKindTag::RBrace).is_some() {
				return Ok(Expr::Plex(Plex {
					name: "unnamed plex".to_string(), //TODO: names?
					fields,
				}));
			}
			
			let name = next_if_eq_err(stream, TokenKindTag::Identifier).map_err(|err| SoftError::Hard(err))?;
			let name = name.kind
				.as_identifier()
				.unwrap()
				.clone();
			
			next_if_eq_err(stream, TokenKindTag::Colon).map_err(|err| SoftError::Hard(err))?;

			if let Ok(r#type) = try_rule("field:expr", stream, observer, parse_expr).shed_hard()? { //ultra temp
				fields.push(Field { name, r#type });
				continue;
			}

			return Err(SoftError::Soft(create_unreachable_error(stream)));
		};
	})
}

fn parse_if<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::If).map_err(|err| SoftError::Soft(err))?;
		
		let cond = try_rule("cond:expr", stream, observer, parse_expr).force_hard()?;

		let body = match next_if_eq(stream, TokenKindTag::Colon) {
			Some(_) => try_rule("body:stmt", stream, observer, parse_stmt).force_hard()?,
			None => try_rule("body:block", stream, observer, parse_block).force_hard()?,
		};

		let body_else = next_if_eq(stream, TokenKindTag::Else).map(|_| {
			try_rule("else:expr", stream, observer, parse_expr)
		}).transpose().force_hard()?;

		Ok(Expr::If(If {
			cond: Box::new(cond),
			body: Box::new(body),
			body_else: body_else.map(|body_else| Box::new(body_else)),
		}))
	})
}

fn parse_loop<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::Loop).map_err(|err| SoftError::Soft(err))?;

		let body = try_rule("body:expr", stream, observer, parse_expr).force_hard()?;
		
		let body_else = next_if_eq(stream, TokenKindTag::Else).map(|_| {
			try_rule("else:expr", stream, observer, parse_expr)
		}).transpose().force_hard()?;

		Ok(Expr::Loop(Loop {
			body: Box::new(body),
			body_else: body_else.map(|body_else| Box::new(body_else)),
		}))
	})
}

fn parse_while<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::While).map_err(|err| SoftError::Soft(err))?;

		let cond = try_rule("cond:expr", stream, observer, parse_expr).force_hard()?;
		
		let body = match next_if_eq(stream, TokenKindTag::Colon) {
			Some(_) => try_rule("body:stmt", stream, observer, parse_stmt).force_hard()?,
			None => try_rule("body:block", stream, observer, parse_block).force_hard()?,
		};

		let body_else = next_if_eq(stream, TokenKindTag::Else).map(|_| {
			try_rule("else:expr", stream, observer, parse_expr)
		}).transpose().force_hard()?;

		Ok(Expr::While(While {
			cond: Box::new(cond),
			body: Box::new(body),
			body_else: body_else.map(|body_else| Box::new(body_else)),
		}))
	})
}

fn parse_for<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::For).map_err(|err| SoftError::Soft(err))?;

		let binding = try_rule("binding:pattern", stream, observer, parse_pattern).force_hard()?;

		next_if_eq_err(stream, TokenKindTag::In).map_err(|err| SoftError::Hard(err))?;

		let iter = try_rule("iter:expr", stream, observer, parse_expr).force_hard()?;
		
		let body = match next_if_eq(stream, TokenKindTag::Colon) {
			Some(_) => try_rule("body:stmt", stream, observer, parse_stmt).force_hard()?,
			None => try_rule("body:block", stream, observer, parse_block).force_hard()?,
		};

		let body_else = next_if_eq(stream, TokenKindTag::Else).map(|_| {
			try_rule("else:expr", stream, observer, parse_expr)
		}).transpose().force_hard()?;

		Ok(Expr::For(For {
			binding: Box::new(binding),
			iter: Box::new(iter),
			body: Box::new(body),
			body_else: body_else.map(|body_else| Box::new(body_else)),
		}))
	})
}

fn parse_expr<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	if let Ok(expr) = try_rule("block", stream, observer, parse_block).shed_hard()? {
		return Ok(expr);
	}
	if let Ok(expr) = try_rule("plex", stream, observer, parse_plex).shed_hard()? {
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

	Err(SoftError::Soft(create_unreachable_error(stream)))
}

fn parse_expr_bin<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node_or_pass(stream, |stream| {
		let lhs = try_rule("expr_una_post", stream, observer, parse_expr_una_post)?;
		
		let stream_recover_state = stream.clone();
		let op = stream.try_rule_opt(parse_bin_op);

		match op {
			None => {
				*stream = stream_recover_state;
				Ok(CreateOrPass::Pass(lhs))
			},
			Some(op) => {
				let mut rhs = parse_expr(stream, observer).force_hard()?;

				if let Expr::BinOp(rhs_bin_op) = &rhs.kind {
					if rhs_bin_op.op.precedence() > op.precedence() {
						let mut rhs_bin_op = rhs.kind.into_bin_op().unwrap(); //safe due to if-let above.

						rhs_bin_op.lhs = Box::new(Node {
							range: source::Range { begin: lhs.range.begin, end: rhs_bin_op.lhs.range.end },
							kind: Expr::BinOp(BinOp {
								op,
								lhs: Box::new(lhs),
								rhs: rhs_bin_op.lhs,
							}),
						});
						rhs = Node {
							range: source::Range { begin: rhs_bin_op.lhs.range.begin, end: rhs_bin_op.rhs.range.end },
							kind: Expr::BinOp(rhs_bin_op),
						};

						Ok(CreateOrPass::Pass(rhs))
					} else {
						Ok(CreateOrPass::Create(Expr::BinOp(BinOp {
							op,
							lhs: Box::new(lhs),
							rhs: Box::new(rhs),
						})))
					}
				} else {
					Ok(CreateOrPass::Create(Expr::BinOp(BinOp {
						op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					})))
				}
			},
		}
	})
}

fn parse_bin_op(stream: &mut impl TokStream<'_>) -> Option<BinOpKind> {
	match stream.next()?.kind {
		TokenKind::Plus => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Add, allow_wrap: next_if_eq(stream, TokenKindTag::Percent).is_some() })),
		TokenKind::Dash => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Sub, allow_wrap: next_if_eq(stream, TokenKindTag::Percent).is_some() })),
		TokenKind::Star => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Mul, allow_wrap: next_if_eq(stream, TokenKindTag::Percent).is_some() })),
		TokenKind::Slash => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Div, allow_wrap: next_if_eq(stream, TokenKindTag::Percent).is_some() })),
		TokenKind::Percent => Some(BinOpKind::Arith(ArithBinOp { kind: ArithBinOpKind::Mod, allow_wrap: false })), //TODO: allow_wrap is not a thing for mod, change data structure?

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

		_ => None,
	}
}

fn parse_expr_una_post<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node_or_pass(stream, |stream| {
		let expr = try_rule("expr_una_pre", stream, observer, parse_expr_una_pre)?;

		let stream_recover_state = stream.clone();
		let Some(token) = stream.next() else {
			return Ok(CreateOrPass::Pass(expr));
		};

		Ok(CreateOrPass::Create(match &token.kind {
			TokenKind::Hash => Expr::UnaOp(UnaOp { op: UnaOpKind::Deref, expr: Box::new(expr) }),
			TokenKind::Ampersand => Expr::UnaOp(UnaOp { op: UnaOpKind::AddressOf, expr: Box::new(expr) }),
			TokenKind::Dot => {
				let ident = parse_identifier_without_crying_myself_to_sleep(stream, observer).force_hard()?;
				Expr::FieldAccess(FieldAccess { expr: Box::new(expr), ident })
			},
			TokenKind::LParen => {
				let arg = try_rule("arg:expr", stream, observer, parse_expr)?;
				next_if_eq_err(stream, TokenKindTag::RParen).map_err(|err| SoftError::Soft(err))?;
				Expr::Call(Call { func: Box::new(expr), arg: Box::new(arg) })
			},
			_ => {
				*stream = stream_recover_state;
				return Ok(CreateOrPass::Pass(expr));
			},
		}))
	})
}

fn parse_expr_una_pre<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node_or_pass(stream, |stream| {
		let op = stream.next_if_map(|t| match t.kind {
			TokenKind::Plus => Some(UnaOpKind::Identity),
			TokenKind::Dash => Some(UnaOpKind::Negate),
			TokenKind::Bang => Some(UnaOpKind::Not),
			_ => None,
		});
		let expr = try_rule("atom", stream, observer, parse_expr_atom)?;
		let Some(op) = op else { return Ok(CreateOrPass::Pass(expr)); };
		let op = op.1;
		Ok(CreateOrPass::Create(Expr::UnaOp(UnaOp { op, expr: Box::new(expr) })))
	})
}

fn parse_expr_atom<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Expr> {
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
	create_node(stream, |stream| stream.try_rule_sh(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::True => Ok(Expr::BoolLit(true)),
			TokenKind::False => Ok(Expr::BoolLit(false)),
			TokenKind::IntLit(value) => Ok(Expr::IntLit(value.clone())),
			TokenKind::StrLit(value) => Ok(Expr::StrLit(value.clone())),
			TokenKind::Identifier(value) => Ok(Expr::Identifier(Rc::new(RefCell::new(Var {
				name: value.clone(),
				id: u64::MAX,
				is_mut: false, //arbitrary, gets set in SEM stage later
			})))),
			_ => Err(SoftError::Soft(RuleErrorKind::UnexpectedToken((&token.kind).into()))),
		},
		None => Err(SoftError::Soft(RuleErrorKind::StreamExhausted)),
	}))
}

fn parse_binding<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	create_node(stream, |stream| Ok(Pattern::Binding(parse_identifier(stream, observer)?)))
}

fn parse_identifier<'a, 'i>(stream: &mut impl TokStream<'i>, _observer: &'a mut impl RuleObserver<'i>) -> SoftResult<Rc<RefCell<Var>>, RuleErrorKind, RuleErrorKind> {
	let ident = next_if_eq_err(stream, TokenKindTag::Identifier)
		.map_err(|err| SoftError::Soft(err))?;

	Ok(Rc::new(RefCell::new(Var {
		name: ident.kind.as_identifier().unwrap().clone(), //unwrap safe due to guard above
		id: u64::MAX,
		is_mut: false, //arbitrary, gets set in SEM stage later
	})))
}

//TODO: this is a whole new level of incompetence...
fn parse_identifier_without_crying_myself_to_sleep<'a, 'i>(stream: &mut impl TokStream<'i>, _observer: &'a mut impl RuleObserver<'i>) -> SoftResult<Rc<Var>, RuleErrorKind, RuleErrorKind> {
	let ident = next_if_eq_err(stream, TokenKindTag::Identifier)
		.map_err(|err| SoftError::Soft(err))?;

	Ok(Rc::new(Var {
		name: ident.kind.as_identifier().unwrap().clone(), //unwrap safe due to guard above
		id: u64::MAX,
		is_mut: false, //arbitrary, gets set in SEM stage later
	}))
}

fn parse_parenthesis<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	next_if_eq_err(stream, TokenKindTag::LParen).map_err(|err| SoftError::Soft(err))?;

	let expr = try_rule("expr", stream, observer, parse_expr).force_hard()?;
	
	next_if_eq_err(stream, TokenKindTag::RParen)
		.map_err(|err| SoftError::Hard(err))?;

	Ok(expr)
}

fn parse_tuple_ctor<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Expr> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::LParen).map_err(|err| SoftError::Soft(err))?;

		let mut items = Vec::new();
		while next_if_eq(stream, TokenKindTag::RParen).is_none() {
			let item = try_rule("item:expr", stream, observer, parse_expr);
			let item = if items.is_empty() { item? } else { item.force_hard()? };
			items.push(item);

			if next_if_eq(stream, TokenKindTag::Comma).is_none() {
				if let Err(err) = next_if_eq_err(stream, TokenKindTag::RParen) {
					return Err(SoftError::Hard(err));
				}
				if items.len() == 1 {
					return Err(SoftError::Soft(RuleErrorKind::UnexpectedToken(TokenKindTag::RParen)));
				}
				break;
			}
		}

		Ok(Expr::TupleCtor(TupleCtor { items }))
	})
}

fn parse_tuple_dtor<'a, 'i>(stream: &mut impl TokStream<'i>, observer: &'a mut impl RuleObserver<'i>) -> RuleResult<Pattern> {
	create_node(stream, |stream| {
		next_if_eq_err(stream, TokenKindTag::LParen).map_err(|err| SoftError::Soft(err))?;

		let mut items = Vec::new();
		while next_if_eq(stream, TokenKindTag::RParen).is_none() {
			let item = try_rule("pattern:arg", stream, observer, parse_pattern);
			let item = if items.is_empty() { item? } else { item.force_hard()? };
			items.push(item);

			if next_if_eq(stream, TokenKindTag::Comma).is_none() {
				if let Err(err) = next_if_eq_err(stream, TokenKindTag::RParen) {
					return Err(SoftError::Hard(err));
				}
				if items.len() == 1 {
					return Err(SoftError::Soft(RuleErrorKind::UnexpectedToken(TokenKindTag::RParen)));
				}
				break;
			}
		}

		Ok(Pattern::TupleDtor(TupleDtor { items }))
	})
}
