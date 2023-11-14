use std::{fmt::{Display}, error::Error};

use error_stack::Report;
use color_print::cformat;

use super::{
	lexer::{Token, TokenKind},
	stream::{Stream, StreamExt},
	source::SourceFile,
};
pub use self::node::{
	NodeKind,
	Node,
	Block,
	Let,
	BinOp,
	MathBinOpKind,
	MathBinOpVariant,
	BinOpKind,
	CmpBinOpKind
};

mod node;

#[derive(Debug)]
pub enum AstError {
	Hard(String),
	Soft(String),
}

impl Error for AstError {

}

impl Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::Hard(err) => write!(f, "{}", cformat!("<red>[Hard]</> {err}")),
            AstError::Soft(err) => write!(f, "{}", cformat!("<yellow>[Soft]</> {err}")),
        }
    }
}

macro_rules! try_hard {
	($expr:expr) => {
		match $expr {
			Ok(ok) => Ok(ok),
			Err(err) => match err.current_context() {
				AstError::Hard(_) => return Err(err),
				AstError::Soft(_) => Err(err),
			}
		}
	};
}

trait TokStreamIter<'i> = Iterator<Item = &'i Token>;
trait TokStreamRF<'i> = for<'a> Fn(&'a &'i Token) -> &'a &'i Token;
trait TokStreamMF<'i> = Fn(&'i Token) -> &'i Token;
type TokStream<'i,
	I/*: TokStreamIter<'i>*/,
	RF/*: TokStreamRF<'i>*/,
	MF/*: TokStreamMF<'i>*/,
> = Stream<I, RF, MF, &'i Token>;


fn rule<'i, I, RF, MF>(
	rule_name: &'static str,
	stream: &mut TokStream<'i, I, RF, MF>,
	f: fn(&mut TokStream<'i, I, RF, MF>) -> Result<Node, Report<AstError>>
) -> Result<Node, Report<AstError>>
where
	I: TokStreamIter<'i> + Clone,
	RF: TokStreamRF<'i> + Clone,
	MF: TokStreamMF<'i> + Clone,
{
	f(stream).map_err(|err| {
			let msg = format!("Rule '{rule_name}'");
			let ctx = match err.current_context() {
				AstError::Hard(_) => AstError::Hard(msg),
				AstError::Soft(_) => AstError::Soft(msg),
			};
			err.change_context(ctx)
		}
	)
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

fn expect_eq_err<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i>,
	impl TokStreamRF<'i>,
	impl TokStreamMF<'i>,
>, expected: TokenKind) -> Result<&'i Token, String> {
	stream.expect_err(|t| if t.kind == expected {
		Ok(())
	} else {
		Err(format!("found '{}'", t.kind.as_ref()))
	}).map_err(|err| format!("Expected '{}' but {}", expected.as_ref(), err))
}

pub fn ast<'a>(source: &'a SourceFile, tokens: &'a Vec<Token>) -> Result<Node, Report<AstError>> {
	let mut stream = tokens.iter().stream(
		|t| t,
		|t| t,
	);

	match parse_file(&mut stream) {
		Ok(t) => Ok(t),
		Err(err) => Err(match stream.get_peeker().get_current() {
			None => err,
			Some(token) => err.attach_printable({
				let source_range = token.source.clone().to_meta(source);
				let source_range_string = format!("[{}]", source_range);
				let (line, error_inset, error_length) = if true {
					let line = source_range.get_line();
					let line_trunc_length = line
							.char_indices()
							.find_map(|(i, c)| (!matches!(c, '\t' | ' ')).then_some(i))
							.unwrap_or(0);
					let line = &line[line_trunc_length..];

					let error_inset = source_range.get_begin().column_index() - line_trunc_length;
					let error_length = source_range.range.get_length();

					(line, error_inset, error_length)
				} else {
					let line = "!!! NO LINE !!!";
					(line, 0, line.len())
				};

				cformat!(
					"<cyan>{empty:>source_range_string_len$}--></><green>{file_path}</>\n\
					<cyan>{empty:>source_range_string_len$} | </>\n\
					<cyan>{source_range_string            } | </>{line}\n\
					<cyan>{empty:>source_range_string_len$} | </><red>{empty:>error_inset$}{empty:^>error_length$}here</>",
					empty = "",
					source_range_string = source_range_string,
					source_range_string_len = source_range_string.len(),
					file_path = source.get_name(),
					line = line,
					error_inset = error_inset,
					error_length = error_length,
				)
			}),
		}),
	}
}

fn parse_file<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if stream.expect(|&t| t.kind == TokenKind::Eof).is_some() {
			return Ok(Node { kind: NodeKind::Block(block) });
		}
		
		if let Ok(stmt) = try_hard!(rule("statement", stream, parse_stmt)) {
			block.stmts.push(stmt);
			continue;
		}

		return Err(Report::new(match stream.get_peeker().get_current() {
			Some(token) => AstError::Soft(format!("Failed to parse file. token: '{}'", token.kind.as_ref())),
			None => AstError::Soft("Stream is exhausted".to_string()),
		}));
	};
}

fn parse_stmt<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	if let Ok(stmt) = try_hard!(rule("let", stream, parse_let)) {
		return Ok(stmt);
	}
	if let Ok(stmt) = try_hard!(rule("assign", stream, parse_assignment)) {
		return Ok(stmt);
	}
	if let Ok(stmt) = try_hard!(rule("block", stream, parse_block)) {
		return Ok(stmt);
	}

	return Err(Report::new(AstError::Soft("Failed to parse stmt".to_string())));
}

fn parse_let<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	if let Err(err) = expect_eq_err(stream, TokenKind::Let) {
		return Err(Report::new(AstError::Soft(err)));
	}
	
	discard_space(stream);
	let lhs = Box::new(expr(stream)?);
	
	discard_space(stream);
	let rhs = if stream.expect(|c| c.kind == TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(expr(stream)?))
	} else {
		None
	};
	
	discard_space(stream);
	expect_eq_err(stream, TokenKind::Semicolon)
		.map_err(|err| Report::new(AstError::Hard(err)))?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_assignment<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	let lhs = match try_hard!(expr(stream)) {
		Ok(lhs) => Box::new(lhs),
		Err(err) => return Err(err.change_context(AstError::Soft("Failed to parse LHS".to_string()))),
	};
	
	discard_space(stream);
	let rhs = if stream.expect(|c| c.kind == TokenKind::Equals).is_some() {
		discard_space(stream);
		Some(Box::new(expr(stream)?))
	} else {
		None
	};
	
	discard_space(stream);
	expect_eq_err(stream, TokenKind::Semicolon)
		.map_err(|err| Report::new(AstError::Hard(err)))?;
	
	Ok(Node { kind: NodeKind::Let(Let { lhs, rhs }) })
}

fn parse_block<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	expect_eq_err(stream, TokenKind::LBrace)
		.map_err(|err| Report::new(AstError::Soft(err)))?;
	
	let mut block = Block::new();
	loop {
		discard_space(stream);

		if stream.expect(|&t| t.kind == TokenKind::RParen).is_some() {
			return Ok(Node { kind: NodeKind::Block(block) });
		}
		
		if let Ok(stmt) = try_hard!(rule("stmt", stream, parse_stmt)) {
			block.stmts.push(stmt);
			continue;
		}

		return Err(Report::new(match stream.get_peeker().get_current() {
			Some(token) => AstError::Soft(format!("Failed to parse block. token: '{}'", token.kind.as_ref())),
			None => AstError::Soft("Stream is exhausted".to_string()),
		}));
	};
}

fn expr<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	let lhs = rule("atom", stream, expr_atom)?;
	
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

	Ok(match op {
		None => {
			stream.pop();
			lhs
		},
		Some((_, op)) => {
			discard_space(stream.get());
			let mut rhs = expr(stream.get())?;
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
	})
}

fn expr_atom<'i>(stream: &mut TokStream<'i,
	impl TokStreamIter<'i> + Clone,
	impl TokStreamRF<'i> + Clone,
	impl TokStreamMF<'i> + Clone,
>) -> Result<Node, Report<AstError>> {
	stream.hypothetically(|stream| match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: NodeKind::IntLit(*value) }),
			TokenKind::StrLit(value) => Ok(Node { kind: NodeKind::StrLit(value.clone()) }),
			TokenKind::Identifier(value) => Ok(Node { kind: NodeKind::Indentifier(value.clone()) }),
			_ => Err(Report::new(AstError::Soft(format!("Unexpected '{}'", token.kind.as_ref())))),
		},
		None => Err(Report::new(AstError::Soft("Stream is exhausted".to_string()))),
	})
}
