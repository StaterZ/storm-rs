#[derive(Debug)]
struct TokenStreamExhaustedError {}

impl Display for TokenStreamExhaustedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Token stream was exhausted")
	}
}

impl Error for TokenStreamExhaustedError {}


#[derive(Debug)]
struct BlockNotClosedError {}

impl Display for BlockNotClosedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Block not closed")
	}
}

impl Error for BlockNotClosedError {}

#[derive(Debug)]
struct UnexpectedTokenError {}

impl Display for UnexpectedTokenError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Unexpected token")
	}
}

impl Error for UnexpectedTokenError {}
