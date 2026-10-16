use std::{cmp::Ordering, fmt::{self, Display}};

use num_bigint::{BigInt, BigUint};
use num_traits::{One, Signed, ToPrimitive, Zero};

use crate::compiler::lexer::literal::RealLit;

/// An exact fraction, always stored in lowest terms with a positive denominator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rational {
	numer: BigInt,
	denom: BigInt,
}

fn gcd(mut a: BigInt, mut b: BigInt) -> BigInt {
	a = a.abs();
	b = b.abs();
	while !b.is_zero() {
		let r = &a % &b;
		a = b;
		b = r;
	}
	a
}

impl Rational {
	pub fn new(numer: BigInt, denom: BigInt) -> Rational {
		assert!(!denom.is_zero(), "a rational can't have a zero denominator");
		let divisor = gcd(numer.clone(), denom.clone());
		let (mut numer, mut denom) = (numer / &divisor, denom / &divisor);
		if denom.is_negative() {
			numer = -numer;
			denom = -denom;
		}
		Rational { numer, denom }
	}

	pub fn from_int(value: BigInt) -> Rational {
		Rational { numer: value, denom: BigInt::one() }
	}

	pub fn from_lit(lit: &RealLit) -> Rational {
		let radix = BigInt::from(lit.radix);
		let mantissa = BigInt::from(lit.mantissa.clone());
		let power = radix.pow(lit.exponent.unsigned_abs() as u32);
		if lit.exponent >= 0 {
			Rational::from_int(mantissa * power)
		} else {
			Rational::new(mantissa, power)
		}
	}

	/// The exact value of a float, which always has a finite binary fraction.
	pub fn from_f64(value: f64) -> Option<Rational> {
		if !value.is_finite() {
			return None;
		}
		let bits = value.to_bits();
		let negative = bits >> 63 == 1;
		let exponent = ((bits >> 52) & 0x7ff) as i64;
		let fraction = bits & ((1 << 52) - 1);
		let (mantissa, exponent) = if exponent == 0 {
			(fraction, -1074) // subnormal
		} else {
			(fraction | (1 << 52), exponent - 1075)
		};
		let mantissa = BigInt::from(mantissa);
		let mantissa = if negative { -mantissa } else { mantissa };
		let power = BigInt::from(2).pow(exponent.unsigned_abs() as u32);
		Some(if exponent >= 0 {
			Rational::from_int(mantissa * power)
		} else {
			Rational::new(mantissa, power)
		})
	}

	pub fn is_zero(&self) -> bool {
		self.numer.is_zero()
	}

	pub fn is_negative(&self) -> bool {
		self.numer.is_negative()
	}

	pub fn add(&self, other: &Rational) -> Rational {
		Rational::new(&self.numer * &other.denom + &other.numer * &self.denom, &self.denom * &other.denom)
	}

	pub fn sub(&self, other: &Rational) -> Rational {
		Rational::new(&self.numer * &other.denom - &other.numer * &self.denom, &self.denom * &other.denom)
	}

	pub fn mul(&self, other: &Rational) -> Rational {
		Rational::new(&self.numer * &other.numer, &self.denom * &other.denom)
	}

	/// `None` when dividing by zero.
	pub fn div(&self, other: &Rational) -> Option<Rational> {
		(!other.is_zero()).then(|| Rational::new(&self.numer * &other.denom, &self.denom * &other.numer))
	}

	/// The remainder of a division rounded toward zero, so it has the sign of `self`.
	pub fn rem(&self, other: &Rational) -> Option<Rational> {
		let quotient = self.div(other)?.trunc();
		Some(self.sub(&other.mul(&Rational::from_int(quotient))))
	}

	pub fn neg(&self) -> Rational {
		Rational { numer: -&self.numer, denom: self.denom.clone() }
	}

	/// Rounds toward zero.
	pub fn trunc(&self) -> BigInt {
		&self.numer / &self.denom
	}

	pub fn to_f64(&self) -> f64 {
		// scale the fraction so its integer part holds enough bits, then let the division round once
		let shift = 64i64 - (self.numer.bits() as i64 - self.denom.bits() as i64);
		let scaled = if shift >= 0 {
			(&self.numer << shift as usize) / &self.denom
		} else {
			&self.numer / (&self.denom << (-shift) as usize)
		};
		let value = scaled.to_f64().unwrap_or(f64::INFINITY);
		value * 2f64.powi(-(shift.clamp(i32::MIN as i64, i32::MAX as i64) as i32))
	}

	/// The value as a real literal, when it's a terminating decimal.
	pub fn to_lit(&self, num_bits: Option<u64>) -> Option<(bool, RealLit)> {
		// a fraction terminates in decimal when its denominator only has the factors 2 and 5
		let mut denom = self.denom.clone();
		let (two, five) = (BigInt::from(2), BigInt::from(5));
		let mut digits = 0u32;
		let mut scale = BigInt::one();
		while !denom.is_one() {
			if (&denom % &two).is_zero() {
				denom /= &two;
				scale *= &five;
			} else if (&denom % &five).is_zero() {
				denom /= &five;
				scale *= &two;
			} else {
				return None;
			}
			digits += 1;
		}
		let mantissa = (&self.numer * scale).abs();
		let (_, mantissa) = mantissa.into_parts();
		let mantissa: BigUint = mantissa;
		Some((self.is_negative(), RealLit { mantissa, radix: 10, exponent: -(digits as i64), num_bits }))
	}
}

impl PartialOrd for Rational {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Rational {
	fn cmp(&self, other: &Self) -> Ordering {
		(&self.numer * &other.denom).cmp(&(&other.numer * &self.denom))
	}
}

impl Display for Rational {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.to_lit(None) {
			Some((negative, lit)) => write!(f, "{}{}", if negative { "-" } else { "" }, lit),
			None => write!(f, "{}/{}", self.numer, self.denom),
		}
	}
}
