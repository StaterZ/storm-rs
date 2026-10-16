use std::io::{self, BufRead, Write};

use color_print::{cformat, cprintln};
use tree_printer;

use super::{lexer::{self, LexerErrorKind}, parser, source};

enum Snippet {
	Done,
	/// The snippet ended early, like an unclosed `{` or string, so more lines are needed.
	Incomplete,
}

/// Reads snippets from stdin and prints their AST.
/// A snippet that ends early keeps reading lines until it's complete, or until an empty line forces it through.
pub fn run() {
	cprintln!("<cyan>Storm interactive mode.</> Enter code to see its AST. Unfinished code continues on the next line, an empty line ends it. Ctrl+Z (Windows) or Ctrl+D quits.");

	let mut lines = io::stdin().lock().lines();
	let mut snippet = String::new();
	loop {
		print!("{}", if snippet.is_empty() { "storm> " } else { "  ...> " });
		let _ = io::stdout().flush();

		let Some(Ok(line)) = lines.next() else {
			println!();
			return;
		};

		let force = line.trim().is_empty();
		if force && snippet.is_empty() {
			continue;
		}
		snippet.push_str(&line);
		snippet.push('\n');

		match show_ast(&snippet, force) {
			Snippet::Done => snippet.clear(),
			Snippet::Incomplete => { },
		}
	}
}

fn show_ast(snippet: &str, force: bool) -> Snippet {
	let document = source::Document::new("<interactive>".to_string(), snippet.to_string());
	let document_meta = source::DocumentMeta::new(&document);

	let tokens = match lexer::lex(&document) {
		Ok(tokens) => tokens,
		Err(err) => {
			let is_incomplete = matches!(err.kind, LexerErrorKind::StringDelimiterNotClosed | LexerErrorKind::MultilineCommentDelimiterNotClosed);
			if is_incomplete && !force {
				return Snippet::Incomplete;
			}
			err.eprint(&document_meta);
			return Snippet::Done;
		},
	};

	match parser::parse(&tokens) {
		Ok(ast) => println!("{}", tree_printer::Tree::new(&ast)
			.label("Root")
			.format_line(|label, value| cformat!("<green>{}</>: {}", label, value))),
		Err(err) if err.is_stream_exhausted() && !force => return Snippet::Incomplete,
		Err(err) => cprintln!("<red>AST Failed:</>\n{}", err.to_meta(&document_meta)),
	}
	Snippet::Done
}
