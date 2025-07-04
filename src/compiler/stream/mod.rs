mod peeker;
mod stream;
mod peekable_iterator;
mod stream_error;
pub mod soft_error;

pub use stream::Stream;
pub use peekable_iterator::PeekableIterator;
pub use stream_error::StreamErrorExpectErr;

use peeker::Peeker;
