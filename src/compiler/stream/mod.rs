pub use stream::Stream;
pub use stream_ext::StreamExt;
pub use stream_hypothetical::StreamHypothetical;

use peeker::Peeker;

mod peeker;
mod stream;
mod stream_ext;
mod stream_hypothetical;
