use inkwell::{context::Context, module::Module, values::BasicValueEnum};

use crate::compiler::parser::nodes::*;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
}

pub fn generate(ast: &Node<Expr>) -> Result<Vec<u8>, GenError> {
	let context = Context::create();
	let builder = context.create_builder();
	let module = context.create_module("main");

	gen_expr(ast, &context, &builder, &module);
	
	let buffer = module.write_bitcode_to_memory();
	Ok(buffer.as_slice().to_vec())
}

fn gen_expr<'ctx>(
	node: &Node<Expr>,
	context: &'ctx Context,
	builder: &inkwell::builder::Builder<'ctx>,
	module: &Module<'ctx>
) -> Option<BasicValueEnum<'ctx>> {
	match &node.kind {
		Expr::Assign(value) => {
			let var_ptr = builder.build_alloca(context.i64_type(), "tmp").unwrap(); //TODO
			let rhs_val = gen_expr(&value.rhs, context, builder, module)?;
			builder.build_store(var_ptr, rhs_val);
			None
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
		Expr::IntLit(value) => Some(context.i64_type().const_int(*value, false).into()),
		Expr::StrLit(value) => {
            let str_const = context.const_string(value.as_bytes(), false);
            let global = module.add_global(str_const.get_type(), None, "str");
            global.set_initializer(&str_const);
            Some(global.as_pointer_value().into())
		},
		Expr::Identifier(_) => todo!(),
	}
}
