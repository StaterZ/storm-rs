pub mod source;
mod lexer;
mod ast;
mod stream;

// type LexResult = Result<(Vec<lexer::Token>, AstResult), String>;
// type AstResult = Result<(ast::Node, SatResult), ast::RuleError>;
// type SatResult = Option<Result<(String, GenResult), String>>;
// type GenResult = Option<Result<String, String>>;

pub struct CompilerOutput {
	pub lex: Option<Result<Vec<lexer::Token>, String>>,
	pub ast: Option<Result<ast::Node, ast::RuleError>>,
	pub sat: Option<Result<String, String>>,
	pub gen: Option<Result<String, String>>,
}

pub fn compile<'a>(src_in: &'a source::Document) -> CompilerOutput {
	let mut result = CompilerOutput {
		lex: None,
		ast: None,
		sat: None,
		gen: None,
	};

	let tokens = lexer::lex(src_in);
	if let Ok(tokens) = &tokens {
		let ast = ast::ast(&tokens);
		if let Ok(_ast) = &ast {
			let sat = Ok("TODO".to_string());
			if let Ok(_sat) = &sat {
				let gen = Ok("TODO".to_string());
				result.gen = Some(gen);
			}
			result.sat = Some(sat);
		}
		result.ast = Some(ast);
	}
	result.lex = Some(tokens);
	
	return result;
}
