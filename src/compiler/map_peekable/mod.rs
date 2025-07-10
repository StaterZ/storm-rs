mod map_peekable;
mod peekable_iterator;
mod peekable_iter_utils;
pub mod soft_error;

pub use map_peekable::MapPeekable;
pub use peekable_iterator::PeekableIterator;
pub use peekable_iter_utils::{PeekableNextIfError, PeekIterUtils};
