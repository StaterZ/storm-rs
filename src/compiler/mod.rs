use error_stack::Report;
use source_meta::SourceFile;

mod lexer;
mod ast;
mod stream;
pub mod source_meta;

pub struct CompilerOutput {
	pub lex: Option<Result<Vec<lexer::Token>, String>>,
	pub ast: Option<Result<ast::Node, Report<ast::AstError>>>,
	pub sat: Option<Result<String, String>>,
	pub gen: Option<Result<String, String>>,
}

pub fn compile<'a>(src_in: &'a SourceFile) -> CompilerOutput {
	let mut result = CompilerOutput {
		lex: None,
		ast: None,
		sat: None,
		gen: None,
	};

	let tokens = lexer::lex(src_in.get_content());
	if let Ok(tokens) = &tokens {
		let ast = ast::ast(&src_in, &tokens);
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
