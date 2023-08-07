use std::fmt::Display;
use enum_as_inner::EnumAsInner;
use lazy_static::__Deref;
use owo_colors::OwoColorize;
use strum::AsRefStr;
use szu::ternary;
use super::super::super::tree_printer::TreeDisplay;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Node>,
}

impl Block {
	pub fn new() -> Self {
		Self { stmts: vec!() }
	}
}

#[derive(Debug)]
pub struct Let {
	pub lhs: Box<Node>,
	pub rhs: Option<Box<Node>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precidence {
	Constant,
	Linear,
	//Exponential,
	Bitwise,
}

#[derive(Debug, PartialEq, Eq, Ord, AsRefStr)]
pub enum MathBinOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Shl,
	Shr,
}

impl MathBinOpKind {
	fn precedence(&self) -> Precidence {
		match self {
			MathBinOpKind::Add | MathBinOpKind::Sub => Precidence::Constant,
			MathBinOpKind::Mul | MathBinOpKind::Div | MathBinOpKind::Mod => Precidence::Linear,
			MathBinOpKind::Shl | MathBinOpKind::Shr => Precidence::Bitwise,
		}
	}
}

impl PartialOrd for MathBinOpKind {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.precedence().cmp(&other.precedence()))
	}
}

#[derive(Debug, PartialEq, Eq, Ord)]
pub struct MathBinOpVariant {
	pub kind: MathBinOpKind,
	pub allow_wrap: bool,
}

impl PartialOrd for MathBinOpVariant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		self.kind.partial_cmp(&other.kind)
	}
}

impl Display for MathBinOpVariant {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}{}", ternary!(self.allow_wrap => "%", ""), self.kind.as_ref())
	}
}

#[derive(Debug, PartialEq, Eq, Ord)]
pub struct CmpBinOpKind {
	pub lt: bool,
	pub eq: bool,
	pub gt: bool,
}

impl PartialOrd for CmpBinOpKind {
	fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
		Some(std::cmp::Ordering::Equal)
	}
}

impl Display for CmpBinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CmpBinOpKind { lt: true, eq: false, gt: false } => f.write_str("<"),
			CmpBinOpKind { lt: true, eq: true, gt: false } => f.write_str("<="),
	
			CmpBinOpKind { lt: false, eq: true, gt: false } => f.write_str("=="),
			CmpBinOpKind { lt: true, eq: false, gt: true } => f.write_str("!="),
	
			CmpBinOpKind { lt: false, eq: false, gt: true } => f.write_str(">"),
			CmpBinOpKind { lt: false, eq: true, gt: true } => f.write_str(">="),
	
			cmp => write!(f, "{:?}", cmp),
		}
	}
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpKind {
	Math(MathBinOpVariant),
	Cmp(CmpBinOpKind),
	Assign(Option<MathBinOpVariant>),
}

impl Display for BinOpKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BinOpKind::Math(math) => f.write_str(math.kind.as_ref()),
			BinOpKind::Cmp(cmp) => f.write_str(cmp.to_string().as_str()),
			BinOpKind::Assign(assign) => f.write_str(assign.as_ref().map_or("none".to_string(), |op| op.to_string()).as_str()),
		}
	}
}

#[derive(Debug)]
pub struct BinOp {
	pub op: BinOpKind,
	pub lhs: Box<Node>,
	pub rhs: Box<Node>
}

#[derive(Debug, AsRefStr, EnumAsInner)]
pub enum NodeKind {
	Block(Block),
	Let(Let),
	BinOp(BinOp),
	IntLit(u64),
	StrLit(String),
	Indentifier(String),
}

#[derive(Debug)]
pub struct Node {
	pub kind: NodeKind,
	//source: SourceRange,
}

impl TreeDisplay for Node {
	fn get_text_line(&self) -> String {
		let text = match &self.kind {
			NodeKind::Block(_) => "".to_string(),
			NodeKind::Let(_) => "".to_string(),
			NodeKind::BinOp(value) => format!("{}", value.op),
			NodeKind::IntLit(value) => format!("{}", value.cyan()),
			NodeKind::StrLit(value) => format!("{:?}", value.cyan()),
			NodeKind::Indentifier(value) => format!("{}", value.cyan()),
		};
		format!("{}{}({})", text, ternary!(text.len() > 0 => " ", ""), self.kind.as_ref())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		match &self.kind {
			NodeKind::Block(value) => Some(
				value.stmts
					.iter()
					.enumerate()
					.map(|(i, stmt)| (format!("[{}]", i), stmt as &dyn TreeDisplay))
					.collect()
			),
			NodeKind::Let(value) => Some(vec![
				("lhs".to_string(), value.lhs.deref()),
				("rhs".to_string(), value.rhs.as_ref().map_or(&"none", |rhs| rhs.deref())),
			]),
			NodeKind::BinOp(value) => Some(vec![
				//("kind".to_string(), &value.op.kind.as_ref()),
				/*("allowWrap".to_string(), match &value.op {
					BinOpKind::Math(math) => ternary!(math.allow_wrap => "true", "false"),
					BinOpKind::Cmp(cmp) => cmp,
				}),*/
				("lhs".to_string(), value.lhs.deref()),
				("rhs".to_string(), value.rhs.deref()),
			]),
			NodeKind::IntLit(_) => None,
			NodeKind::StrLit(_) => None,
			NodeKind::Indentifier(_) => None,
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

/*impl<T: TreeDisplay> TreeDisplay for Option<T> {
	fn get_text_line(&self) -> String {
		self.map_or("none".magenta(), |some| some.get_text_line())
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		self.map(|some| some.get_children())
	}
}

impl<T: TreeDisplay> TreeDisplay for Box<T> {
	fn get_text_line(&self) -> String {
		self.deref().get_text_line()
	}

	fn get_children(&self) -> Option<Vec<(String, &dyn TreeDisplay)>> {
		self.deref().get_children()
	}
}*/
