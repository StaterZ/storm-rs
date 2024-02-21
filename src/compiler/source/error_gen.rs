use color_print::cformat;

use super::{
	RangeMeta,
	Line,
};

pub fn generate_error_line(range: RangeMeta) -> String {
	let range_str = format!("[{}]", range);
	
	let line = {
		let begin = range.get_begin();
		let begin_line = begin.line();
		
		let end = range.get_end();
		
		let last = (begin < end).then(|| end - 1); //begin < end implicitly also ensures end > 0, since begin can't go lower than 0 and end need to be larger than begin
		let last_line = last.and_then(|last| last.line());

		match (begin_line, last_line) {
			(Some(begin_line), Some(last_line)) if begin_line == last_line =>
				Ok(begin_line.range().get_str()),
			(None, None) => Ok(Line::new(range.document.get_lines_begin_indices().len() - 1).to_meta(range.document).range().get_str()), //If it's eof, we give the last line
			_ => Err("MUTI-LINE NOT SUPPORTED"),
		}
	};

	let (line, mut error_inset, error_length) = match line {
		Ok(line) => {
			let line_trunc_length = line
				.char_indices()
				.find_map(|(i, c)| (!matches!(c, '\t' | ' ')).then_some(i))
				.unwrap_or(0);
			let line_trunc_end = line
				.char_indices()
				.rev()
				.find_map(|(i, c)| (!matches!(c, '\r' | '\n')).then_some(i))
				.unwrap_or(line.bytes().len() - 1);
			let line = &line[line_trunc_length..=line_trunc_end];

			let error_inset = range.get_begin().column_raw().index() - line_trunc_length;
			let error_length = range.get_length();

			(line, error_inset, error_length)
		},
		Err(msg) => (msg, 0, msg.len()),
	};


	let error_str =  if error_length > 0 {
		(0..error_length).map(|_| '^').collect::<String>()
	} else {
		error_inset -= 1;
		"▕▏".to_string()
	};

	cformat!(
		"<cyan>{empty:>range_str_len$}--></><green>{file_path}</>\n\
		 <cyan>{empty:>range_str_len$} | </>\n\
		 <cyan>{range_str            } | </>{line}\n\
		 <cyan>{empty:>range_str_len$} | </><red>{empty:>error_inset$}{error_str}here</>\n\
		 <cyan>{empty:>range_str_len$} | </>",
		empty = "",
		range_str_len = range_str.len(),
		file_path = range.document.get_name(),
	)
}
