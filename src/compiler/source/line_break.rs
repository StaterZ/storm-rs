/// Finds the first line break in `text`, returning its byte index and byte length.
/// Line breaks are `\n` (Linux), `\r\n` (Windows) and `\r` (old Mac).
pub fn find_line_break(text: &str) -> Option<(usize, usize)> {
	let index = text.find(['\r', '\n'])?;
	let len = if text[index..].starts_with("\r\n") { 2 } else { 1 };
	Some((index, len))
}
