mod peeker;
mod stream;
mod stream_ext;
mod stream_hypothetical;
mod stream_error;

pub use stream::Stream;
pub use stream_ext::StreamExt;
pub use stream_hypothetical::StreamHypothetical;
pub use stream_error::{StreamErrorExpectErr/*, StreamErrorExpectErrEq*/};

use peeker::Peeker;
