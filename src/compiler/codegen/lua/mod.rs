use color_print::cformat;
use itertools::Itertools;

use crate::compiler::parser::nodes::*;

#[derive(Debug, strum::AsRefStr)]
pub enum GenError {
}

pub fn generate(ast: &Node<Expr>) -> Result<Vec<u8>, GenError> {
	Ok(gen_expr(ast, 0).into_bytes())
}

fn gen_expr(node: &Node<Expr>, loop_depth: usize) -> String {
	match &node.kind {
		Expr::Assign(value) => format!("{} = {}",
			gen_pattern(&value.lhs),
			gen_expr(&value.rhs, loop_depth)),
		Expr::Return(value) => value.expr.as_ref().map_or("return".to_string(),
			|expr| format!("return {}", gen_expr(&expr, loop_depth))),
		Expr::Break(value) => value.expr.as_ref().map_or("goto __brk{loop_depth}".to_string(),
			|expr| format!("__val={};goto __brk{loop_depth}", gen_expr(&expr, loop_depth))),
		Expr::Continue => "goto __cont{loop_depth}".to_string(),
		Expr::Unreachable => "print(\"unreachable\")".to_string(),
		Expr::Loop(value) => value.body_else.as_ref().map_or(
			format!("while true do \n{} \nend", gen_expr(&value.body, loop_depth + 1)),
			|body_else| format!("local __val;\nwhile true do \n{} \n::__cont{loop_depth}:: end \n__val={};::__brk{loop_depth}::",
				gen_expr(&value.body, loop_depth + 1),
				gen_expr(&body_else, loop_depth))),
		Expr::While(value) => value.body_else.as_ref().map_or(
			format!("while {} do \n{} \nend \n::__brk{loop_depth}::",
				gen_expr(&value.cond, loop_depth),
				gen_expr(&value.body, loop_depth + 1)),
			|body_else| format!("local __val;\nwhile {} do \n{} \n::__cont{loop_depth}:: end \n__val={};::__brk{loop_depth}::",
				gen_expr(&value.cond, loop_depth),
				gen_expr(&value.body, loop_depth + 1),
				gen_expr(&body_else, loop_depth))),
		Expr::For(value) => value.body_else.as_ref().map_or(
			format!("do \nlocal __iter={};local __iter_next=__iter.next(); \nwhile __iter_next ~= nil do \nlocal {}=__iter_next;\n{}\n;::__cont{loop_depth}::;__iter_next=__iter.next() \nend \n::__brk{loop_depth}:: \nend",
				gen_expr(&value.iter, loop_depth),
				gen_pattern(&value.binding),
				gen_expr(&value.body, loop_depth + 1)),
			|body_else| format!("do \nlocal __iter={};local __iter_next=__iter.next();local __val;\n while __iter_next ~= nil do\n local {}=__iter_next;\n{}\n;::__cont{loop_depth}::;__iter_next=__iter.next() \nend \n__val={};::__brk{loop_depth}:: \nend",
				gen_expr(&value.iter, loop_depth),
				gen_pattern(&value.binding),
				gen_expr(&value.body, loop_depth + 1),
				gen_expr(&body_else, loop_depth))),
		Expr::If(value) => value.body_else.as_ref().map_or(
			format!("if {} then\n {} \nend",
				gen_expr(&value.cond, loop_depth),
				gen_expr(&value.body, loop_depth + 1)),
			|body_else| format!("do \nlocal __val;\nif {} then \n__val={} \nelse \n__val={} \nend \nend",
				gen_expr(&value.cond, loop_depth),
				gen_expr(&value.body, loop_depth + 1),
				gen_expr(&body_else, loop_depth))),
		Expr::Block(value) => format!("do \n{} \nend", value.stmts
			.iter()
			.map(|stmt| gen_expr(stmt, loop_depth))
			.join("\n")),
		Expr::Stmt(value) => gen_expr(&value.expr, loop_depth),
		Expr::BinOp(value) => format!("{}{}{}", gen_expr(&value.lhs, loop_depth), gen_binop(value.op), gen_expr(&value.rhs, loop_depth)),
		Expr::UnaOp(value) => format!("{}{}", gen_unaop(value.op), gen_expr(&value.expr, loop_depth)),
		Expr::FieldAccess(value) => format!("{}.{}", gen_expr(&value.expr, loop_depth), &value.ident.name),
		Expr::TupleCtor(value) => value.items
			.iter()
			.map(|item| gen_expr(item, loop_depth))
			.join(","),
		Expr::IntLit(value) => value.to_string(),
		Expr::StrLit(value) => value.clone(),
		Expr::Identifier(value) => value.borrow().name.clone(),
	}
}

fn gen_pattern(node: &Node<Pattern>) -> String {
	let mut meta = PatternMeta::new();
	let pat = gen_pattern_impl(node, false, &mut meta);

	//TODO: redo later in pattern: local x,y,z; x,w,y,z=thingy

	match (meta.has_set, !meta.decls.is_empty()) {
		(false, false) => pat,
		(true, false) => pat,
		(false, true) => format!("local {}", pat),
		(true, true) => format!("local {};{}", meta.decls.join(","), pat),
	}
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

fn gen_pattern_impl(node: &Node<Pattern>, is_let: bool, meta: &mut PatternMeta) -> String {
	match &node.kind {
		Pattern::Let(value) => gen_pattern_impl(&value.pat, true, meta),
		Pattern::Mut(value) => gen_pattern_impl(&value.pat, is_let, meta),
		Pattern::TupleDtor(value) => value.items
			.iter()
			.map(|item| gen_pattern_impl(item, is_let, meta))
			.join(","),
		Pattern::Deref(_value) => cformat!("<red>TODO</>"),
		Pattern::Binding(value) => {
			let binding = value.borrow().name.clone();
			if is_let {
				meta.decls.push(binding.clone());
			} else {
				meta.has_set = true;
			}
			binding
		},
	}
}

fn gen_binop(op: BinOpKind) -> String {
	match op {
		BinOpKind::Arith(op) => match op.kind {
			ArithBinOpKind::Add => "+".to_string(),
			ArithBinOpKind::Sub => "-".to_string(),
			ArithBinOpKind::Mul => "*".to_string(),
			ArithBinOpKind::Div => "/".to_string(),
			ArithBinOpKind::Mod => cformat!("<red>TODO</>"),
		},
		BinOpKind::Bitwise(op) => match op {
			BitwiseBinOpKind::Shl => cformat!("<red>TODO</>"),
			BitwiseBinOpKind::Shr => cformat!("<red>TODO</>"),
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

fn gen_unaop(op: UnaOpKind) -> String {
	match op {
		UnaOpKind::Deref => cformat!("<red>TODO</>"),
		UnaOpKind::AddressOf => cformat!("<red>TODO</>"),
	}
}
