use std::ops::Deref;

use itertools::Itertools;

use crate::compiler::{parser::{node_sets::*, nodes::*}, source::Sourced};

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
}

pub fn generate(ast: &Sourced<Expr>) -> Result<Vec<u8>, GenError> {
	Ok(gen_expr(ast, 0).into_bytes())
}

fn gen_expr(node: &Sourced<Expr>, loop_depth: usize) -> String {
	match node.deref() {
		Expr::Assign(node) => {
			let lhs = gen_pattern(&node.lhs, loop_depth);
			let rhs = gen_expr(&node.rhs, loop_depth);
			match lhs {
				Some(lhs) => format!("{} = {}", lhs, rhs),
				None => rhs,
			}
		},
		Expr::Return(node) => node.expr.as_ref().map_or("return".to_string(),
			|expr| format!("return {}", gen_expr(&expr, loop_depth))),
		Expr::Break(node) => node.expr.as_ref().map_or("goto __brk{loop_depth}".to_string(),
			|expr| format!("__val={};goto __brk{loop_depth}", gen_expr(&expr, loop_depth))),
		Expr::Continue => "goto __cont{loop_depth}".to_string(),
		Expr::Unreachable => "print(\"unreachable\")".to_string(),

		Expr::Plex(_) => todo!(),

		Expr::Loop(node) => format!("while true do\n{}\nend", gen_body(&node.body, loop_depth + 1)),
		Expr::While(node) => node.body_else.as_ref().map_or(
			format!("while {} do\n{}\nend\n::__brk{loop_depth}::",
				gen_expr(&node.cond, loop_depth),
				gen_body(&node.body, loop_depth + 1)),
			|body_else| format!("local __val;\nwhile {} do\n{}\n::__cont{loop_depth}:: end\n__val={};::__brk{loop_depth}::",
				gen_expr(&node.cond, loop_depth),
				gen_body(&node.body, loop_depth + 1),
				gen_body(&body_else, loop_depth))),
		Expr::For(node) => node.body_else.as_ref().map_or(
			format!("do\nlocal __iter={};local __iter_next=__iter.next();\nwhile __iter_next ~= nil do\n{}\n{}\n;::__cont{loop_depth}::;__iter_next=__iter.next()\nend\n::__brk{loop_depth}::\nend",
				gen_expr(&node.iter, loop_depth),
				gen_pattern(&node.binding, loop_depth + 1).map_or_default(|pat| format!("local {}=__iter_next;", pat)),
				gen_body(&node.body, loop_depth + 1)),
			|body_else| format!("do\nlocal __iter={};local __iter_next=__iter.next();local __val;\nwhile __iter_next ~= nil do\n{}\n{}\n;::__cont{loop_depth}::;__iter_next=__iter.next()\nend\n__val={};::__brk{loop_depth}::\nend",
				gen_expr(&node.iter, loop_depth),
				gen_pattern(&node.binding, loop_depth + 1).map_or_default(|pat| format!("local {}=__iter_next;", pat)),
				gen_body(&node.body, loop_depth + 1),
				gen_body(&body_else, loop_depth))),
		Expr::If(node) => node.body_else.as_ref().map_or(
			format!("if {} then\n{}\nend",
				gen_expr(&node.cond, loop_depth),
				gen_body(&node.body, loop_depth + 1)),
			|body_else| format!("do\nlocal __val;\nif {} then\n__val={}\nelse\n__val={}\nend\nend",
				gen_expr(&node.cond, loop_depth),
				gen_body(&node.body, loop_depth + 1),
				gen_body(&body_else, loop_depth))),

		Expr::Block(node) => {
			let stmts = node.stmts
				.iter()
				.map(|stmt| gen_expr(stmt, loop_depth))
				.join("\n");
			let expr = node.expr
				.as_ref()
				.map(|expr| gen_expr(expr, loop_depth));
			if let Some(expr) = expr {
				format!("do\n{}\nTODO[{}]\nend", stmts, expr)
			} else {
				format!("do\n{}\nend", stmts)
			}
		},
		Expr::Stmt(node) => gen_expr(&node.expr, loop_depth),
		Expr::BinOp(node) => format!("{}{}{}", gen_expr(&node.lhs, loop_depth), gen_binop(node.op), gen_expr(&node.rhs, loop_depth)),
		Expr::UnaOp(node) => {
			let expr = gen_expr(&node.expr, loop_depth);
			match node.op {
				UnaOpKind::Deref => format!("TODO[{}]", expr),
				UnaOpKind::AddressOf => format!("TODO[{}]", expr),
				UnaOpKind::Identity => format!("+{}", expr),
				UnaOpKind::Negate => format!("-{}", expr),
				UnaOpKind::Not => format!("not {}", expr),
			}
		},
		Expr::FieldAccess(node) => format!("{}.{}", gen_expr(&node.expr, loop_depth), &node.ident.name),
		Expr::Func(node) => format!("function({})\n{}\nend\n", gen_pattern(&node.binding, loop_depth).unwrap_or_default(), gen_body(&node.body, loop_depth)),
		Expr::Call(node) => format!("{}({})", gen_expr(&node.func, loop_depth), gen_expr(&node.arg, loop_depth)),

		Expr::TupleCtor(node) => node.items
			.iter()
			.map(|item| gen_expr(item, loop_depth))
			.join(","),
		Expr::BoolLit(node) => node.to_string(),
		Expr::IntLit(node) => node.to_string(),
		Expr::StrLit(node) => format!("\"{}\"", node.escape_default()),
		Expr::Identifier(node) => node.borrow().name.clone(),
	}
}

fn gen_body(node: &Sourced<Expr>, loop_depth: usize) -> String {
	if let Expr::Block(node) = node.deref() {
		let stmts = node.stmts
			.iter()
			.map(|stmt| gen_expr(stmt, loop_depth))
			.join("\n");
		let expr = node.expr
			.as_ref()
			.map(|expr| gen_expr(expr, loop_depth));
		if let Some(expr) = expr {
			format!("{}\nreturn {}", stmts, expr)
		} else {
			stmts
		}
	} else {
		gen_expr(node, loop_depth)
	}
}

fn gen_pattern(node: &Sourced<Pattern>, loop_depth: usize) -> Option<String> {
	let mut meta = PatternMeta::new();
	let pat = gen_pattern_impl(node, false, loop_depth, &mut meta);

	Some(match (meta.has_set, !meta.decls.is_empty()) {
		(false, false) => return None,
		(true, false) => pat,
		(false, true) => format!("local {}", pat),
		(true, true) => format!("local {};{}", meta.decls.join(","), pat),
	})
}

struct PatternMeta {
	has_set: bool,
	decls: Vec<String>,
}
impl PatternMeta {
	pub fn new() -> Self {
		Self {
			has_set: false,
			decls: Vec::new(),
		}
	}
}

fn gen_pattern_impl(node: &Sourced<Pattern>, is_let: bool, loop_depth: usize, meta: &mut PatternMeta) -> String {
	match node.deref() {
		Pattern::Let(node) => gen_pattern_impl(&node.pat, true, loop_depth, meta),
		Pattern::Mut(node) => gen_pattern_impl(&node.pat, is_let, loop_depth, meta),
		Pattern::TupleDtor(node) => node.items
			.iter()
			.map(|item| gen_pattern_impl(item, is_let, loop_depth, meta))
			.join(","),
		Pattern::Deref(node) => format!("TODO[{}]", gen_expr(node, loop_depth)),
		Pattern::Binding(node) => {
			let binding = node.borrow().name.clone();
			if is_let {
				meta.decls.push(binding.clone());
			} else {
				meta.has_set = true;
			}
			binding
		},
		Pattern::Discard => "_".to_string(),
	}
}

fn gen_binop(op: BinOpKind) -> String {
	match op {
		BinOpKind::Arith(op) => match op.kind {
			ArithBinOpKind::Add => "+".to_string(),
			ArithBinOpKind::Sub => "-".to_string(),
			ArithBinOpKind::Mul => "*".to_string(),
			ArithBinOpKind::Div => "/".to_string(),
			ArithBinOpKind::Mod => "TODO".to_string(),
		},
		BinOpKind::Bitwise(op) => match op {
			BitwiseBinOpKind::Shl => "TODO".to_string(),
			BitwiseBinOpKind::Shr => "TODO".to_string(),
		},
		BinOpKind::Cmp(op) => match op {
			CmpBinOpKind::Eq => "==".to_string(),
			CmpBinOpKind::Ne => "~=".to_string(),
			CmpBinOpKind::Lt => "<".to_string(),
			CmpBinOpKind::Le => "<=".to_string(),
			CmpBinOpKind::Gt => ">".to_string(),
			CmpBinOpKind::Ge => ">=".to_string(),
		},
		BinOpKind::Logic(op) => match op {
			LogicBinOpKind::And => " and ".to_string(),
			LogicBinOpKind::Or => " or ".to_string(),
		},
	}
}
