use color_print::cformat;
use szu::iter::FindLastMapExt;
use unicode_width::UnicodeWidthStr;

use super::RangeMeta;

pub fn generate_error_line(range: RangeMeta) -> String {
	let range_str = format!("[{}]", range);
	
	let line = {
		let begin = range.get_begin();
		let begin_line = begin.line();
		
		let last = range.get_last();
		let last_line = last.map_or(begin_line, |last| last.line()); //if it's None, that means it's zero size, so then we just grab the begin line

		(begin_line == last_line).then_some(begin_line).ok_or("MUTI-LINE NOT SUPPORTED")
	};

	let (line_str, mut error_inset, error_length) = match line {
		Ok(line) => {
			let line_str = line.range().get_str();
			let line_trunc_begin_byte = line_str
				.char_indices()
				.find_map(|(i, c)| (!matches!(c, '\t' | ' ')).then_some(i))
				.unwrap_or(0);
			let line_trunc_end_byte = line_str
				.char_indices()
				.rev()
				.find_last_map(|(i, c)| matches!(c, '\r' | '\n').then_some(i))
				.unwrap_or(line_str.bytes().len());
			
			let error_inset = RangeMeta::new(
				line.range().get_begin().add_byte_offset(line_trunc_begin_byte),
				range.get_begin()
			).get_str().width();
			let error_length = range.get_str().width();
			let line_trunc_str = &line_str[line_trunc_begin_byte..line_trunc_end_byte];
			
			(line_trunc_str, error_inset, error_length)
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
		 <cyan>{range_str            } | </>{line_str}\n\
		 <cyan>{empty:>range_str_len$} | </><red>{empty:>error_inset$}{error_str}here</>\n\
		 <cyan>{empty:>range_str_len$} | </>",
		empty = "",
		range_str_len = range_str.len(),
		file_path = range.document.get_name(),
	)
}
