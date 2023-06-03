use core::slice::Iter;
use lazy_static::__Deref;
use owo_colors::OwoColorize;
use strum::AsRefStr;
use szu::t;
use super::{
	lexer::{Token, TokenKind},
	stream::Stream,
	super::tree_printer::TreeDisplay
};

#[derive(Debug, AsRefStr)]
enum MathBinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Shr,
	Shl,
}

#[derive(Debug)]
struct MathBinOpVariant {
	kind: MathBinOpKind,
	allow_wrap: bool,
}

#[derive(Debug)]
struct CmpBinOpKind {
	lt: bool,
	eq: bool,
	gt: bool,
}

#[derive(Debug)]
enum BinOpKind {
	Math(MathBinOpVariant),
	Cmp(CmpBinOpKind),
}

#[derive(Debug, AsRefStr)]
enum Kind {
	Block { stmts: Vec<Node> },
	Let { lhs: Box<Node>, rhs: Box<Node> },
	BinOp { op: BinOpKind, lhs: Box<Node>, rhs: Box<Node> },
	IntLit { value: u64 },
	StrLit { value: String },
	Indentifier { value: String },
}

#[derive(Debug)]
pub struct Node {
	kind: Kind,
	//source: SourceRange,
}

impl TreeDisplay for Node {
	fn get_text_line(&self) -> String {
		let text = match &self.kind {
			Kind::Block { stmts: _ } => "".to_string(),
			Kind::Let { lhs: _, rhs: _ } => "".to_string(),
			Kind::BinOp { op, lhs: _, rhs: _ } => match op {
				BinOpKind::Math(math) => math.kind.as_ref().to_string(),
				BinOpKind::Cmp(cmp) => match cmp {
					CmpBinOpKind { lt: true, eq: false, gt: false } => "<",
					CmpBinOpKind { lt: true, eq: true, gt: false } => "<=",

					CmpBinOpKind { lt: false, eq: true, gt: false } => "==",
					CmpBinOpKind { lt: true, eq: false, gt: true } => "!=",

					CmpBinOpKind { lt: false, eq: false, gt: true } => ">",
					CmpBinOpKind { lt: false, eq: true, gt: true } => ">=",

					cmp => panic!("bad {:?}", cmp),
				}.to_string(),
			},
			Kind::IntLit { value } => format!("{}", value.cyan()),
			Kind::StrLit { value } => format!("{:?}", value.cyan()),
			Kind::Indentifier { value } => format!("{}", value.cyan()),
		};
		format!("{}{}({})", text, t!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		match &self.kind {
			Kind::Block { stmts } => Some(
				stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i), stmt as &dyn TreeDisplay))
					.collect()
			),
			Kind::Let { lhs, rhs } => Some(vec![
				("lhs".to_string(), lhs.deref()),
				("rhs".to_string(), rhs.deref()),
			]),
			Kind::BinOp { op, lhs, rhs } => Some(vec![
				//("kind".to_string(), &op.kind.as_ref()),
				//("allowWrap".to_string(), &op.allow_wrap),
				("lhs".to_string(), lhs.deref()),
				("rhs".to_string(), rhs.deref()),
			]),
			Kind::IntLit { value: _ } => None,
			Kind::StrLit { value: _ } => None,
			Kind::Indentifier { value: _ } => None,
		}
	}
}

impl<T: std::fmt::Display> TreeDisplay for T {
	fn get_text_line(&self) -> String {
		format!("{}", self.cyan())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		None
	}
}

fn discard_space(stream: &mut Stream<Iter<Token>>) {
	while stream.expect(|&t| matches!(t.kind, 
		TokenKind::Space |
		TokenKind::NewLine |
		TokenKind::Comment |
		TokenKind::MultilineComment
	)).is_some() { };
}

fn stream_expect_token_kind<'a>(stream: &'a mut Stream<Iter<Token>>, kind: TokenKind) -> Result<&'a Token, String> {
	stream.expect_err(|&t| if t.kind == kind {
		Ok(())
	} else {
		Err(format!("Expected token '{}' but found '{}'", kind.as_ref(), t.kind.as_ref()))
	})
}

pub fn ast(tokens: &Vec<Token>) -> Result<Node, String> {
	let mut stream = Stream::new(tokens.iter());
	let mut stmts = vec!();
	loop {
		discard_space(&mut stream);
		match stream.next() {
			Some(token) => match token.kind {
				TokenKind::Eof => return Ok(Node { kind: Kind::Block { stmts } }),
				TokenKind::Let => {
					discard_space(&mut stream);
					let lhs = Box::new(expr(&mut stream)?);

					discard_space(&mut stream);
					stream_expect_token_kind(&mut stream, TokenKind::Equals)?;

					discard_space(&mut stream);
					let rhs = Box::new(expr(&mut stream)?);

					stmts.push(Node { kind: Kind::Let { lhs, rhs } });

					stream_expect_token_kind(&mut stream, TokenKind::Semicolon)?;
				},
				_ => return Err(format!("Unexpected token '{}' found", token.kind.as_ref())),
			},
			None => return Err("No token found".to_string()),
		};
	};
}

fn expr(stream: &mut Stream<Iter<Token>>) -> Result<Node, String> {
	let lhs = expr_atom(stream)?;
	
	let mut stream = stream.dup();
	discard_space(stream.get());
	if stream.get().expect(|&t| t.kind == TokenKind::Compare).is_none() {
		stream.pop();
		return Ok(lhs);
	}
	
	discard_space(stream.get());
	let rhs = expr_atom(stream.get())?;
	stream.nip();

	return Ok(Node {
		kind: Kind::BinOp { op: BinOpKind::Cmp(CmpBinOpKind { lt: false, eq: true, gt: false }),
			lhs: Box::new(lhs),
			rhs: Box::new(rhs),
		}
	});
}

fn expr_atom(stream: &mut Stream<core::slice::Iter<Token>>) -> Result<Node, String> {
	match stream.next() {
		Some(token) => match &token.kind {
			TokenKind::IntLit(value) => Ok(Node { kind: Kind::IntLit { value: *value } }),
			TokenKind::StrLit(value) => Ok(Node { kind: Kind::StrLit { value: value.clone() } }),
			TokenKind::Identifier(value) => Ok(Node { kind: Kind::Indentifier { value: value.clone() } }),
			_ => Err(format!("Unexpected token '{}' found", token.kind.as_ref())),
		},
		None => Err("No token found".to_string()),
	}
}
