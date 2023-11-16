use super::FilePos;

pub trait FilePosExt : Iterator<Item = char> {
	fn file_pos(self) -> FilePos<Self>
	where
		Self: Sized
	{
		FilePos::new(self)
	}
}

impl<I: Iterator<Item = char>> FilePosExt for I {}
