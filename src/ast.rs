#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstError {
	BadNumber(std::num::ParseIntError),
}

#[derive(Debug)]
pub enum Expr {
	Error,
	Lit(Lit),
	UnaOp(UnaOp, Box<Expr>),
	BinOp(Box<Expr>, BinOp, Box<Expr>),
	Let { binding: String, expr: Box<Expr> },
}

#[derive(Debug)]
pub enum Lit {
	Int(i32),
	Float(f32),
	String(String),
	Ident(String),
}

#[derive(Debug)]
pub enum UnaOp {
	Identity,
	Negate,
	Not,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinOp {
	Arith(ArithBinOp),
	Logic(LogicBinOp),
	Cmp(CmpBinOp),
}


#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ArithBinOp {
	Mul,
	Div,
	Add,
	Sub,
	//Rem,
	//Mod,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LogicBinOp {
	And,
	Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CmpBinOp {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
}
