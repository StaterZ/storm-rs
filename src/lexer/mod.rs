use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
	#[default] Geh,
}

#[derive(Logos, logos_display::Debug, logos_display::Display, PartialEq, Clone)]
#[logos(error = LexingError, skip r"[ \t\n\f]+", skip r"//[^/][^\n]*", skip r"/\*(?:[^*]|\*[^/])*\*/")]
pub enum Token {
	#[token("let")]
	KeywordLet,
	#[token("fn")]
	KeywordFn,
	#[token("ret")]
	KeywordRet,
	#[token("struct")]
	KeywordStruct,
	#[token("if")]
	KeywordIf,
	#[token("else")]
	KeywordElse,
	#[token("while")]
	KeywordWhile,
	#[token("for")]
	KeywordFor,
	#[token("match")]
	KeywordMatch,
	#[token("pub")]
	KeywordPub,
	#[token("mut")]
	KeywordMut,

	// Modern way of allowing identifiers, read: https://unicode.org/reports/tr31/
	#[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())]
	Identifier(String),
	#[regex(r"\d+", |lex| lex.slice().parse::<u128>().unwrap(), priority = 2)]
	Integer(u128),
	#[regex(r#""(?:[^"]|\\")*""#, |lex| {
		let slice = lex.slice();
		let len = slice.len();
		unescaper::unescape(&slice[1..(len-1)]).expect("failed to unescape string")
	})]
	String(String),
	
	#[token("(")]
	LParen,
	#[token(")")]
	RParen,
	#[token("{")]
	LCurlyBracket,
	#[token("}")]
	RCurlyBracket,
	#[token("[")]
	LSquareBracket,
	#[token("]")]
	RSquareBracket,
	#[token("=")]
	Assign,
	#[token(";")]
	Semicolon,
	#[token(":")]
	Colon,
	#[token(",")]
	Coma,
	#[token(".")]
	Dot,
	#[token("<")]
	LessThanSign,
	#[token(">")]
	MoreThanSign,
	#[token(">=")]
	MoreThanEqSign,
	#[token("<=")]
	LessThanEqSign,

	#[token("+")]
	OperatorAdd,
	#[token("-")]
	OperatorSub,
	#[token("*")]
	OperatorMul,
	#[token("/")]
	OperatorDiv,
	#[token("%")]
	OperatorRem,
	#[token("&&")]
	OperatorAnd,
	#[token("||")]
	OperatorOr,
	#[token("==")]
	OperatorEq,
	#[token("!=")]
	OperatorNe,
	#[token("!")]
	OperatorNot,
}
