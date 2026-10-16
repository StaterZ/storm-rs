use std::fmt::{self, Display};

use num_bigint::BigUint;
use num_traits::{ToPrimitive, Zero};

use super::{lexer_error::LexerErrorKind, Token};

/// The type an int literal is suffixed with, like `12i64`, `12u8` or `1b1`. Any number of bits is allowed, like `u7`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
	Signed(u64),
	Unsigned(u64),
	Bool(u64),
}

impl Display for IntType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			IntType::Signed(bits) => write!(f, "i{}", bits),
			IntType::Unsigned(bits) => write!(f, "u{}", bits),
			IntType::Bool(bits) => write!(f, "b{}", bits),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntLit {
	pub value: BigUint,
	pub ty: Option<IntType>,
}

impl Display for IntLit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)?;
		if let Some(ty) = self.ty {
			write!(f, "{}", ty)?;
		}
		Ok(())
	}
}

/// An exact real literal, its value is `mantissa * radix^exponent`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RealLit {
	pub mantissa: BigUint,
	pub radix: u32,
	pub exponent: i64,
	/// The bits of an `f` suffix, like 32 for `1.5f32`.
	pub num_bits: Option<u64>,
}

impl RealLit {
	/// The closest f64, losing precision.
	pub fn to_f64(&self) -> f64 {
		let mantissa = self.mantissa.to_f64().unwrap_or(f64::INFINITY);
		let exponent = self.exponent.clamp(i32::MIN as i64, i32::MAX as i64) as i32;
		mantissa * (self.radix as f64).powi(exponent)
	}
}

impl Display for RealLit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.radix {
			10 => write!(f, "{}e{}", self.mantissa, self.exponent)?,
			radix => write!(f, "{}*{}^{}", self.mantissa.to_str_radix(radix), radix, self.exponent)?,
		}
		if let Some(bits) = self.num_bits {
			write!(f, "f{}", bits)?;
		}
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumLit {
	Int(IntLit),
	Real(RealLit),
}

impl Display for NumLit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			NumLit::Int(lit) => write!(f, "{}", lit),
			NumLit::Real(lit) => write!(f, "{}", lit),
		}
	}
}

pub(super) fn lex_num(lex: &mut logos::Lexer<Token>) -> Result<NumLit, LexerErrorKind> {
	// the regex can't tell `1e-3` from `0x1e - 3`, so a signed exponent is only taken in for decimal literals
	let slice = lex.slice();
	let ends_in_exponent = slice.ends_with(['e', 'E']) && parse_radix(slice)?.0 == 10;
	let rest = lex.remainder();
	if ends_in_exponent && rest.starts_with(['+', '-']) && rest[1..].starts_with(|c: char| c.is_ascii_digit()) {
		let len = rest[1..]
			.find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
			.map_or(rest.len(), |len| len + 1);
		lex.bump(len);
	}

	parse_num(lex.slice())
}

/// Parses a number literal like `0x1f`, `36rzz`, `1_000u8` or `1.5e-3f32`.
pub fn parse_num(s: &str) -> Result<NumLit, LexerErrorKind> {
	let (radix, body) = parse_radix(s)?;

	let (int_digits, mut rest) = take_digits(body, radix);
	check_digits(int_digits, rest, radix)?;
	let mut last_digits = int_digits;

	let mut frac_digits = None;
	if let Some(after_dot) = rest.strip_prefix('.') {
		let (digits, after_digits) = take_digits(after_dot, radix);
		check_digits(digits, after_digits, radix)?;
		frac_digits = Some(digits);
		last_digits = digits;
		rest = after_digits;
	}

	let mut exponent = None;
	if radix == 10 && let Some(after_e) = rest.strip_prefix(['e', 'E']) {
		let (negative, after_sign) = match after_e.strip_prefix('-') {
			Some(after_sign) => (true, after_sign),
			None => (false, after_e.strip_prefix('+').unwrap_or(after_e)),
		};
		let (digits, after_digits) = take_digits(after_sign, 10);
		if digits.is_empty() {
			return Err(LexerErrorKind::ExponentHasNoDigits);
		}
		check_digits(digits, after_digits, 10)?;
		let value = digits_value(digits, 10)
			.to_i64()
			.ok_or(LexerErrorKind::ExponentTooLarge)?;
		exponent = Some(if negative { -value } else { value });
		last_digits = digits;
		rest = after_digits;
	}

	let suffix = parse_suffix(rest, radix)?;
	if suffix.is_none() && last_digits.ends_with('_') {
		return Err(LexerErrorKind::NumHasTrailingUnderscore);
	}

	let is_real = frac_digits.is_some() || exponent.is_some() || matches!(suffix, Some(Suffix::Float(_)));
	if is_real {
		let bits = match suffix {
			None => None,
			Some(Suffix::Float(bits)) => Some(bits),
			Some(Suffix::Int(ty)) => return Err(LexerErrorKind::RealWithIntSuffix(ty)),
		};
		let frac_digits = frac_digits.unwrap_or("");
		let frac_len = frac_digits.chars().filter(|&c| c != '_').count() as i64;
		let mantissa = digits_value(&format!("{}{}", int_digits, frac_digits), radix);
		let exponent = exponent
			.unwrap_or(0)
			.checked_sub(frac_len)
			.ok_or(LexerErrorKind::ExponentTooLarge)?;
		return Ok(NumLit::Real(RealLit { mantissa, radix, exponent, num_bits: bits }));
	}

	let value = digits_value(int_digits, radix);
	let ty = match suffix {
		None => None,
		Some(Suffix::Int(ty)) => Some(ty),
		Some(Suffix::Float(_)) => unreachable!("float suffixes make the literal real"),
	};
	if let Some(ty) = ty && !fits(&value, ty) {
		return Err(LexerErrorKind::NumDoesNotFit { value, ty });
	}
	Ok(NumLit::Int(IntLit { value, ty }))
}

/// Splits off the radix prefix: `0b` 2, `0q` 4, `0o` 8, `0d` 10, `0x` 16, or `<radix>r` for any radix from 2 to 36. Defaults to 10.
pub fn parse_radix(s: &str) -> Result<(u32, &str), LexerErrorKind> {
	if let Some(rest) = s.strip_prefix('0') {
		let radix = match rest.chars().next() {
			Some('b') => Some(2),
			Some('q') => Some(4),
			Some('o') => Some(8),
			Some('d') => Some(10),
			Some('x') => Some(16),
			_ => None,
		};
		if let Some(radix) = radix {
			return Ok((radix, &rest[1..]));
		}
	}

	let (digits, rest) = take_digits(s, 10);
	let Some(rest) = rest.strip_prefix('r') else {
		return Ok((10, s));
	};
	check_digits(digits, rest, 10)?;
	if digits.ends_with('_') {
		return Err(LexerErrorKind::NumHasTrailingUnderscore);
	}
	let radix = digits_value(digits, 10);
	match radix.to_u32() {
		Some(radix @ 2..=36) => Ok((radix, rest)),
		Some(0 | 1) => Err(LexerErrorKind::RadixTooSmall(radix)),
		_ => Err(LexerErrorKind::RadixTooLarge(radix)),
	}
}

enum Suffix {
	Int(IntType),
	Float(u64),
}

/// Parses a type suffix like `i64`, `u7`, `f32` or `b1`, if there is one.
fn parse_suffix(s: &str, radix: u32) -> Result<Option<Suffix>, LexerErrorKind> {
	let Some(kind) = s.chars().next() else {
		return Ok(None);
	};
	if !matches!(kind, 'i' | 'u' | 'f' | 'b') {
		return Err(invalid_char(s, radix));
	}

	let bits = &s[1..];
	if bits.is_empty() || !bits.chars().all(|c| c.is_ascii_digit()) {
		return Err(LexerErrorKind::InvalidSuffix(s.to_string()));
	}
	let bits = digits_value(bits, 10)
		.to_u64()
		.ok_or_else(|| LexerErrorKind::InvalidSuffix(s.to_string()))?;

	Ok(Some(match kind {
		'i' => Suffix::Int(IntType::Signed(bits)),
		'u' => Suffix::Int(IntType::Unsigned(bits)),
		'b' if bits > 0 => Suffix::Int(IntType::Bool(bits)),
		'f' if bits > 0 => Suffix::Float(bits),
		_ => return Err(LexerErrorKind::InvalidSuffix(s.to_string())),
	}))
}

/// Splits off the leading digits valid for `radix`, including `_` separators.
fn take_digits(s: &str, radix: u32) -> (&str, &str) {
	let len = s
		.find(|c: char| !(c == '_' || c.is_digit(radix)))
		.unwrap_or(s.len());
	s.split_at(len)
}

fn check_digits(digits: &str, rest: &str, radix: u32) -> Result<(), LexerErrorKind> {
	if digits.starts_with('_') {
		return Err(LexerErrorKind::DigitsHaveLeadingUnderscore);
	}
	if digits.is_empty() {
		return Err(match rest.chars().next() {
			Some(c) if c.is_ascii_alphanumeric() => invalid_char(rest, radix),
			_ => LexerErrorKind::NumHasNoDigits,
		});
	}
	Ok(())
}

fn invalid_char(s: &str, radix: u32) -> LexerErrorKind {
	let c = s.chars().next().expect("callers check that there's a char");
	match c.to_digit(36) {
		Some(digit_value) => LexerErrorKind::InvalidDigitForRadix { digit_symbol: c, digit_value, radix },
		None => LexerErrorKind::InvalidSuffix(s.to_string()),
	}
}

fn digits_value(digits: &str, radix: u32) -> BigUint {
	let digits = digits.bytes().filter(|&c| c != b'_').collect::<Vec<_>>();
	if digits.is_empty() {
		return BigUint::zero();
	}
	BigUint::parse_bytes(&digits, radix).expect("digits are checked against the radix before parsing")
}

fn fits(value: &BigUint, ty: IntType) -> bool {
	match ty {
		IntType::Unsigned(bits) => value.bits() <= bits,
		// the sign is a separate token, so the magnitude can reach 2^(bits-1) to allow the smallest value
		IntType::Signed(0) => value.is_zero(),
		IntType::Signed(bits) => value.bits() < bits || (value.bits() == bits && value.trailing_zeros() == Some(bits - 1)),
		IntType::Bool(_) => value.bits() <= 1,
	}
}

#[cfg(test)]
mod tests;
