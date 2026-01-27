use std::{collections::HashMap, convert::identity, ops::Deref, path::PathBuf, str::FromStr};

use inkwell::{
	builder::{Builder, BuilderError},
	context::Context,
	module::{Linkage, Module},
	targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
	types::{BasicType, BasicTypeEnum},
	values::{BasicValue, FunctionValue},
	AddressSpace,
	IntPredicate
};
use itertools::Itertools;

use crate::compiler::{parser::{node_sets::*, nodes::*}, source::Sourced};
use value::*;

mod value;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
	BuilderError(BuilderError)
}

struct FuncJob<'a, 'ctx> {
	value: FunctionValue<'ctx>,
	node: &'a Func,
}

pub fn generate(ast: &Sourced<Expr>) -> Result<Vec<u8>, GenError> {
	let context = Context::create();
	let builder = context.create_builder();
	let module = context.create_module("main");

	let i8ptr_type = context.ptr_type(AddressSpace::default());
	let printf_type = context.i32_type().fn_type(&[i8ptr_type.into()], true);
	let print_fn = module.add_function("printf", printf_type, Some(Linkage::External));

	let i8ptr_type = context.ptr_type(AddressSpace::default());
	let scanf_type = context.i32_type().fn_type(&[i8ptr_type.into()], true);
	let scan_fn = module.add_function("scanf", scanf_type, Some(Linkage::External));
	
	let mut func_tbl = Vec::new();
	{
		// Create a dummy main function to emit blocks into
		let main_fn_type = context.i32_type().fn_type(&[], false);
		let main_fn = module.add_function("main", main_fn_type, None);

		let entry = context.append_basic_block(main_fn, "entry");
		builder.position_at_end(entry);
	
		let mut sym_tbl = HashMap::new();
		decl_extern(print_fn, &builder, &mut sym_tbl); //TODO: super temp
		decl_extern(scan_fn, &builder, &mut sym_tbl); //TODO: super temp

		let expr_val = gen_expr(&ast, &context, &builder, &module, &mut sym_tbl, &mut func_tbl).map_err(|err| GenError::BuilderError(err))?;
		builder.build_return(expr_val
			.as_ref()
			.map(|expr_val| expr_val as &dyn BasicValue)).map_err(|err| GenError::BuilderError(err))?;
	}

	let err: Result<(), BuilderError> = try {
		while let Some(job) = func_tbl.pop() {			
			let entry = context.append_basic_block(job.value, "entry");
			builder.position_at_end(entry);

			let mut sym_tbl = HashMap::new();
			decl_extern(print_fn, &builder, &mut sym_tbl); //TODO: super temp
			decl_extern(scan_fn, &builder, &mut sym_tbl); //TODO: super temp

			let rhs = job.value.get_first_param().unwrap().into();
			gen_pattern(&job.node.binding, rhs, true, &context, &builder, &module, &mut sym_tbl, &mut func_tbl)?;
			
			let expr_val = gen_expr(&job.node.body, &context, &builder, &module, &mut sym_tbl, &mut func_tbl)?;
			builder.build_return(expr_val
				.as_ref()
				.map(|expr_val| expr_val as &dyn BasicValue))?;
		}
	};
	err.map_err(|err| GenError::BuilderError(err))?;

	Ok(stupid_shit(&module))
}

fn decl_extern<'ctx>(
	func: FunctionValue<'ctx>,
	builder: &Builder<'ctx>,
	sym_tbl: &mut HashMap<String, PtrValue<'ctx>>,
) {
	let print_fn_ptr = PtrValue::new(
		func.as_global_value().as_pointer_value(),
		ValueType::Func(FuncType::new(func)),
	);
	let print_fn_ptr_ptr = builder.build_alloca(print_fn_ptr.clone().get_type(), "print_fn_ptr").expect("no alloca?");
	let _ = builder.build_store(print_fn_ptr_ptr, print_fn_ptr.clone());
	sym_tbl.insert(func.get_name().to_str().unwrap().to_string(), PtrValue::new(print_fn_ptr_ptr, ValueType::Ptr(Box::new(print_fn_ptr.get_type()))));
}

fn gen_expr<'a, 'ctx>(
	node: &'a Sourced<Expr>,
	context: &'ctx Context,
	builder: &Builder<'ctx>,
	module: &Module<'ctx>,
	sym_tbl: &mut HashMap<String, PtrValue<'ctx>>,
	func_tbl: &mut Vec<FuncJob<'a, 'ctx>>,
) -> Result<Option<Value<'ctx>>, BuilderError> {
	match node.deref() {
		Expr::Assign(value) => {
			if value.op.is_some() { todo!(); }

			let rhs = gen_expr(&value.rhs, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			gen_pattern(&value.lhs, rhs, false, context, builder, module, sym_tbl, func_tbl)?;
			Ok(None)
		},
		Expr::Return(value) => {
			let expr_val = value.expr
				.as_ref()
				.map(|expr| Ok::<_, BuilderError>(gen_expr(expr, context, builder, module, sym_tbl, func_tbl)?.unwrap()))
				.transpose()?;

			builder.build_return(expr_val
				.as_ref()
				.map(|expr_val| expr_val as &dyn BasicValue))?;
			
			Ok(None)
		},
		Expr::Break(_value) => {
			todo!();
			// let expr_val = value.expr
			// 	.as_ref()
			// 	.map(|expr| Ok(gen_expr(expr, context, builder, module, sym_tbl, func_tbl)?.unwrap()))
			// 	.transpose()?;
			//Ok(None)
		},
		Expr::Continue => {
			todo!();
			//Ok(None)
		},
		Expr::Unreachable => {
			builder.build_unreachable()?;
			Ok(None)
		},

		Expr::Plex(_) => todo!(),

		Expr::Loop(body) => {
			//TODO: i ain't trusting this mess yet

			let parent = builder.get_insert_block().unwrap().get_parent().unwrap();
			let loop_bb = context.append_basic_block(parent, "loop");
			let after_bb = context.append_basic_block(parent, "after_loop");

			builder.build_unconditional_branch(loop_bb)?;
			builder.position_at_end(loop_bb);

			gen_expr(&body.body, context, builder, module, sym_tbl, func_tbl)?;
			builder.build_unconditional_branch(loop_bb)?;

			builder.position_at_end(after_bb);
			Ok(None)
		},
		Expr::While(value) => {
			//TODO: i ain't trusting this mess yet

			let parent = builder.get_insert_block().unwrap().get_parent().unwrap();
			let cond_bb = context.append_basic_block(parent, "cond");
			let body_bb = context.append_basic_block(parent, "while_body");
			let end_bb = context.append_basic_block(parent, "end");

			builder.build_unconditional_branch(cond_bb)?;
			builder.position_at_end(cond_bb);

			let cond = gen_expr(&value.cond, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			let cond = cond.into_int().unwrap(); // assume i1 result
			builder.build_conditional_branch(cond, body_bb, end_bb)?;

			builder.position_at_end(body_bb);
			gen_expr(&value.body, context, builder, module, sym_tbl, func_tbl)?;
			builder.build_unconditional_branch(cond_bb)?;

			builder.position_at_end(end_bb);
			Ok(None)
		},
		Expr::For(_) => {
			//TODO: i ain't trusting this mess yet
			todo!()
		},
		Expr::If(value) => {
			//TODO: i ain't trusting this mess yet

			let parent = builder.get_insert_block().unwrap().get_parent().unwrap();
			let then_bb = context.append_basic_block(parent, "then");
			let else_bb = context.append_basic_block(parent, "else");
			let end_bb = context.append_basic_block(parent, "end");

			let cond = gen_expr(&value.cond, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			builder.build_conditional_branch(cond.into_int().unwrap(), then_bb, else_bb)?;

			builder.position_at_end(then_bb);
			let then_val = gen_expr(&value.body, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			builder.build_unconditional_branch(end_bb)?;

			builder.position_at_end(else_bb);
			let else_val = value.body_else
				.as_ref()
				.map(|body_else| Ok::<_, BuilderError>(gen_expr(body_else.as_ref(), context, builder, module, sym_tbl, func_tbl)?.unwrap()))
				.transpose()?;
			builder.build_unconditional_branch(end_bb)?;

			builder.position_at_end(end_bb);
			let Some(else_val) = else_val else { return Ok(None); };

			let phi = builder.build_phi(context.i64_type(), "if_temp")?;
			phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
			Ok(Some(Value::Phi(phi)))
		},

		Expr::Block(value) => {
			for stmt in value.stmts.iter() {
				let stmt = gen_expr(stmt, context, builder, module, sym_tbl, func_tbl)?;
				assert_eq!(stmt, None);
			}
			let expr = value.expr
				.as_ref()
				.map(|expr| gen_expr(expr, context, builder, module, sym_tbl, func_tbl))
				.transpose()?
				.and_then(identity);
			Ok(expr)
		},
		Expr::Stmt(value) => gen_expr(&value.expr, context, builder, module, sym_tbl, func_tbl),
		Expr::BinOp(value) => {
			let lhs = gen_expr(&value.lhs, context, builder, module, sym_tbl, func_tbl)?.unwrap().into_int().unwrap();
			let rhs = gen_expr(&value.rhs, context, builder, module, sym_tbl, func_tbl)?.unwrap().into_int().unwrap();
			let is_signed = true;
			
			let result = match value.op {
				BinOpKind::Arith(op) => match op.kind {
					ArithBinOpKind::Add => builder.build_int_add(lhs, rhs, "add_temp")?,
					ArithBinOpKind::Sub => builder.build_int_sub(lhs, rhs, "sub_temp")?,
					ArithBinOpKind::Mul => builder.build_int_mul(lhs, rhs, "mul_temp")?,
					ArithBinOpKind::Div => if is_signed { builder.build_int_signed_div(lhs, rhs, "div_temp")? } else { builder.build_int_unsigned_div(lhs, rhs, "div_temp")? },
					ArithBinOpKind::Mod => if is_signed {
						let rem = builder.build_int_signed_rem(lhs, rhs, "rem_temp")?;
						
						//TODO: performance?
						let zero = lhs.get_type().const_zero();
						let is_negative = builder.build_int_compare(IntPredicate::SLT, rem, zero, "rem_is_negative")?;
						let rhs_is_negative = builder.build_int_compare(IntPredicate::SLT, rhs, zero, "rhs_is_negative")?;
						let needs_fix = builder.build_int_compare(IntPredicate::NE, is_negative, rhs_is_negative, "needs_fix")?;
						let fixed_rem = builder.build_int_add(rem, rhs, "fixed_rem")?;
						builder.build_select(needs_fix, fixed_rem, rem, "floor_mod")?.into_int_value()
					} else { builder.build_int_unsigned_rem(lhs, rhs, "rem_temp")? },
				},
				BinOpKind::Bitwise(op) => match op {
					BitwiseBinOpKind::Shl => builder.build_left_shift(lhs, rhs, "shl_temp")?,
					BitwiseBinOpKind::Shr => builder.build_right_shift(lhs, rhs, is_signed, "shr_temp")?,
				},
				BinOpKind::Cmp(op) => match op {
					CmpBinOpKind::Eq => builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eq_temp")?,
					CmpBinOpKind::Ne => builder.build_int_compare(IntPredicate::NE, lhs, rhs, "ne_temp")?,
					CmpBinOpKind::Lt => builder.build_int_compare(if is_signed { IntPredicate::SLT } else { IntPredicate::ULT }, lhs, rhs, "lt_temp")?,
					CmpBinOpKind::Le => builder.build_int_compare(if is_signed { IntPredicate::SLE } else { IntPredicate::ULE }, lhs, rhs, "le_temp")?,
					CmpBinOpKind::Gt => builder.build_int_compare(if is_signed { IntPredicate::SGT } else { IntPredicate::UGT }, lhs, rhs, "gt_temp")?,
					CmpBinOpKind::Ge => builder.build_int_compare(if is_signed { IntPredicate::SGE } else { IntPredicate::UGE }, lhs, rhs, "ge_temp")?,
				},
				BinOpKind::Logic(op) => match op {
					LogicBinOpKind::And => builder.build_and(lhs, rhs, "and_temp")?,
					LogicBinOpKind::Or => builder.build_or(lhs, rhs, "bor_temp")?,
				},
			};
			Ok(Some(Value::Int(result)))
		},
		Expr::UnaOp(value) => {
			let expr_val = gen_expr(&value.expr, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			let result = match value.op {
				UnaOpKind::Deref => {
					let ptr = expr_val.into_ptr().unwrap();
					builder.build_load(ptr.get_pointee_type().clone(), *ptr, "deref")?.into()
				},
				UnaOpKind::AddressOf => {
					if let Expr::Identifier(value) = &**value.expr {
						let binding = &value.borrow().name;
						Value::Ptr(sym_tbl[binding.as_str()].clone())
					} else {
						let ty = expr_val.get_type();
						let ptr = builder.build_alloca(ty.clone(), "alloc_temp")?;
						Value::Ptr(PtrValue::new(ptr, ty))
					}
				},
				UnaOpKind::Identity => expr_val,
				UnaOpKind::Negate => {
					let expr_val = expr_val.into_int().unwrap();
					Value::Int(builder.build_int_neg(expr_val, "neg_temp")?)
				},
				UnaOpKind::Not => {
					let int_val = expr_val.into_int().unwrap();
					Value::Int(builder.build_not(int_val, "not_temp")?)
				},
			};
			Ok(Some(result))
		},
		Expr::FieldAccess(_) => todo!(),
		Expr::Func(value) => {
			let arg_ty = gen_pattern_type(&value.binding, context)?;
			
			let fn_type = context.i64_type().fn_type(&[arg_ty.into()], false);
			let func = module.add_function("lambda", fn_type, None);
			func_tbl.push(FuncJob { value: func, node: value });

			//goopy awfulness:
			let fn_ptr = PtrValue::new(
				func.as_global_value().as_pointer_value(),
				ValueType::Func(FuncType::new(func)),
			);
			Ok(Some(Value::Ptr(fn_ptr)))
		},
		Expr::Call(value) => {
			let func_ptr = gen_expr(&value.func, context, builder, module, sym_tbl, func_tbl)?.unwrap().into_ptr().unwrap();
			let func = func_ptr
				.get_pointee_type()
				.as_func()
				.unwrap()
				.clone();
			let arg = gen_expr(&value.arg, context, builder, module, sym_tbl, func_tbl)?.unwrap();
			let args: Vec<inkwell::values::BasicMetadataValueEnum<'_>> = if func.get_linkage() == Linkage::External {
				let arg = arg
					.into_ptr()
					.unwrap();
				arg
					.get_pointee_type()
					.as_struct()
					.unwrap()
					.get_field_types_iter()
					.enumerate()
					.map(|(i, ty)| Ok({
						let field_ptr = builder.build_struct_gep(arg.get_pointee_type().clone(), *arg, i as u32, "call_arg_ptr")?;
						builder.build_load(ty, field_ptr, "call_arg")?.into()
					}))
					.collect::<Result<Vec<_>, BuilderError>>()?
			} else {
				vec![arg.into()]
			};
			let call_site = builder.build_indirect_call(
				func.into(),
				*func_ptr,
				&args,
				"call",
			)?;
			Ok(Some(call_site.try_as_basic_value().unwrap_basic().into()))
		},

		Expr::TupleCtor(value) => {
			let item_values = value.items
				.iter()
				.map(|item| Ok(gen_expr(item, context, builder, module, sym_tbl, func_tbl)?.unwrap()))
				.collect::<Result<Vec<_>, BuilderError>>()?;

			let tuple_type = context.struct_type(
				&item_values
					.iter()
					.map(|v| v.clone().get_type().as_basic_type_enum())
					.collect::<Vec<_>>(),
				false,
			);
			let tuple_alloca = builder.build_alloca(tuple_type, "tuple_ctor")?;

			for (i, value) in item_values.into_iter().enumerate() {
				let gep = builder.build_struct_gep(
					tuple_type,
					tuple_alloca,
					i as u32,
					"tuple_ctor_elem"
				)?;
				builder.build_store(gep, value)?;
			}

			Ok(Some(Value::Ptr(PtrValue::new(tuple_alloca, ValueType::Struct(tuple_type)))))
		},
		Expr::BoolLit(value) => Ok(Some(Value::Int(context.bool_type().const_int(*value as u64, false)))),
		Expr::IntLit(value) => Ok(Some(Value::Int(context.i64_type().const_int(*value, false)))),
		Expr::StrLit(value) => {
			let str_const = context.const_string(value.as_bytes(), true); //false
			let global = module.add_global(str_const.get_type(), None, "str");
			global.set_initializer(&str_const);
			Ok(Some(Value::Ptr(PtrValue::new(global.as_pointer_value(), ValueType::Array(str_const.get_type())))))
		},
		Expr::Identifier(value) => {
			let binding = &value.borrow().name;
			let sym = sym_tbl[binding.as_str()].clone();
			let val = builder.build_load(sym.get_pointee_type().clone(), *sym, binding.as_str())?;
			Ok(Some(Value::new(val, sym.get_pointee_type().clone())))
		}
	}
}

fn gen_pattern<'a, 'ctx>(
	node: &'a Sourced<Pattern>,
	rhs: Value<'ctx>,
	is_let: bool, 
	context: &'ctx Context,
	builder: &Builder<'ctx>,
	module: &Module<'ctx>,
	sym_tbl: &mut HashMap<String, PtrValue<'ctx>>,
	func_tbl: &mut Vec<FuncJob<'a, 'ctx>>,
) -> Result<(), BuilderError> {
	match node.deref() {
		Pattern::Let(value) => gen_pattern(&value.pat, rhs, true, context, builder, module, sym_tbl, func_tbl),
		Pattern::Mut(value) => gen_pattern(&value.pat, rhs, is_let, context, builder, module, sym_tbl, func_tbl),
		Pattern::TupleDtor(value) => {
			let rhs = rhs.into_struct().unwrap();
			let n = rhs.get_type().get_field_types().len();
			let fields = (0..n).map(|i| builder.build_extract_value(rhs, i as u32, "tuple_dtor_elem"));
			
			for (lhs_item, rhs_item) in value.items.iter().zip_eq(fields) {
				gen_pattern(lhs_item, rhs_item?.into(), is_let, context, builder, module, sym_tbl, func_tbl)?;
			}
			Ok(())
		},
		Pattern::Deref(value) => {
			let ptr = gen_expr(value, context, builder, module, sym_tbl, func_tbl)?.unwrap().into_ptr().unwrap();
			builder.build_store(*ptr, rhs)?;
			Ok(())
		},
		Pattern::Binding(value) => {
			let binding = value.borrow().name.clone();
			let ptr = if is_let {
				let ty = rhs.clone().get_type();
				let ptr = builder.build_alloca(ty.clone(), binding.as_str())?;
				sym_tbl.insert(binding, PtrValue::new(ptr, ty));
				ptr
			} else {
				sym_tbl[binding.as_str()].deref().clone()
			};

			builder.build_store(ptr, rhs)?;
			Ok(())
		},
		Pattern::Discard => Ok(()),
	}
}
fn gen_pattern_type<'ctx>(
	node: &Sourced<Pattern>,
	context: &'ctx Context,
) -> Result<ValueType<'ctx>, BuilderError> {
	match node.deref() {
		Pattern::Let(value) => gen_pattern_type(&value.pat, context),
		Pattern::Mut(value) => gen_pattern_type(&value.pat, context),
		Pattern::TupleDtor(value) => {
			let field_types = value.items
				.iter()
				.map(|item| Ok(gen_pattern_type(item, context)?.into()))
				.collect::<Result<Vec<BasicTypeEnum>, BuilderError>>()?;

			Ok(ValueType::Struct(context.struct_type(field_types.as_slice(), false)))
		},
		Pattern::Deref(_value) => {
			//Ok(ValueType::Ptr(gen_expr(value, context)?.unwrap().into_ptr().unwrap().get_type()))
			todo!()
		},
		Pattern::Binding(_value) => {
			Ok(ValueType::Int(context.i64_type())) //TODO: types, self evident really...
		},
		Pattern::Discard => todo!("yikes! use of discard in declaration"),
	}
}

fn stupid_shit<'ctx>(module: &Module<'ctx>) -> Vec<u8> {
	println!("Starting Backend...");
	let out_dir_path = PathBuf::from_str("out").unwrap();
	if let Err(e) = std::fs::create_dir_all(&out_dir_path) {
		if e.kind() != std::io::ErrorKind::AlreadyExists {
			eprintln!("Failed to create folder: {}", e);
		}
	}

	let ir_path = out_dir_path.join("test.ll");
	module.print_to_file(&ir_path).unwrap();
	
	Target::initialize_all(&Default::default());
	let target_triple = TargetMachine::get_default_triple();
	let target = Target::from_triple(&target_triple).unwrap();
	let target_machine = target
		.create_target_machine(
			&target_triple,
			"generic",
			"",
			inkwell::OptimizationLevel::Default,
			RelocMode::Default,
			CodeModel::Default,
		)
		.unwrap();

	println!("LLVM is running...");
	let object_path = out_dir_path.join("test.o");
	target_machine
		.write_to_file(&module, FileType::Object, &object_path)
		.unwrap();

	let exe_path = out_dir_path.join("test.exe");
	{
		println!("Spinning up CLANG...");
		let args = vec![
			"-o", exe_path.as_os_str().to_str().unwrap(),
			"-target", "x86_64-w64-mingw32",
			object_path.as_os_str().to_str().unwrap(),
		];
		let mut linker = std::process::Command::new("clang");
		let proc = linker.args(args.iter()).spawn().unwrap();
		let output = proc.wait_with_output().unwrap();
		println!("Linker status: {}", output.status.success());
		if !output.status.success() { return Vec::new() }
	}

	{
		println!("Executable is running...");
		let mut exe = std::process::Command::new(&exe_path);
		let proc = exe.spawn().unwrap();
		let output = proc.wait_with_output().unwrap();
		let code = output.status.code().unwrap();
		println!("Exit code was: {}", code);
		Vec::new()
	}
}
