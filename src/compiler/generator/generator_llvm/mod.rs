use inkwell::context::Context;

use crate::compiler::parser::nodes::*;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	TodoErr,
}

pub fn generate(ast: &Node<Expr>) -> Result<String, GenError> {
	let context = Context::create();
	let builder = context.create_builder();
	
	gen_expr(ast, &context, &builder);
	
	Ok("TODO".to_string())
}

fn gen_expr(node: &Node<Expr>, context: &Context, builder: &inkwell::builder::Builder<'_>) -> _ {
	let x = match node.kind {
		Expr::Assign(value) => {
			builder.build_store(ptr, gen_expr(value.rhs, context, builder))
		},
		Expr::Return(_) => todo!(),
		Expr::Break(_) => todo!(),
		Expr::Continue => todo!(),
		Expr::Unreachable => todo!(),
		Expr::Loop(_) => todo!(),
		Expr::While(_) => todo!(),
		Expr::For(_) => todo!(),
		Expr::If(_) => todo!(),
		Expr::Block(_) => todo!(),
		Expr::Stmt(_) => todo!(),
		Expr::BinOp(_) => todo!(),
		Expr::UnaOp(_) => todo!(),
		Expr::FieldAccess(_) => todo!(),
		Expr::TupleCtor(_) => todo!(),
		Expr::IntLit(value) => context.i64_type().const_int(value, false),
		Expr::StrLit(value) => context.const_string(value.as_bytes(), false),
		Expr::Identifier(_) => todo!(),
	}
}
