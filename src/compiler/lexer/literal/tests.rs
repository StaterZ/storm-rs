use num_bigint::BigUint;

use super::{parse_num, parse_radix, IntLit, IntType::*, IntType, LexerErrorKind::*, LexerErrorKind, NumLit, RealLit};

fn big(value: &str) -> BigUint {
	value.parse().unwrap()
}

fn int(value: &str, ty: Option<IntType>) -> Result<NumLit, LexerErrorKind> {
	Ok(NumLit::Int(IntLit { value: big(value), ty }))
}

fn real(mantissa: &str, radix: u32, exponent: i64, bits: Option<u64>) -> Result<NumLit, LexerErrorKind> {
	Ok(NumLit::Real(RealLit { mantissa: big(mantissa), radix, exponent, num_bits: bits }))
}

fn suffix(text: &str) -> Result<NumLit, LexerErrorKind> {
	Err(InvalidSuffix(text.to_string()))
}

fn digit(digit_symbol: char, digit_value: u32, radix: u32) -> Result<NumLit, LexerErrorKind> {
	Err(InvalidDigitForRadix { digit_symbol, digit_value, radix })
}

fn no_fit(value: &str, ty: IntType) -> Result<NumLit, LexerErrorKind> {
	Err(NumDoesNotFit { value: big(value), ty })
}

#[track_caller]
fn check(cases: &[(&str, Result<NumLit, LexerErrorKind>)]) {
	for (text, expected) in cases {
		assert_eq!(&parse_num(text), expected, "parsing {:?}", text);
	}
}

#[test]
fn radix() {
	assert_eq!(parse_radix("123"), Ok((10, "123")));
	assert_eq!(parse_radix("0b101"), Ok((2, "101")));
	assert_eq!(parse_radix("0q123"), Ok((4, "123")));
	assert_eq!(parse_radix("0o777"), Ok((8, "777")));
	assert_eq!(parse_radix("0d99"), Ok((10, "99")));
	assert_eq!(parse_radix("0xff"), Ok((16, "ff")));
	assert_eq!(parse_radix("0"), Ok((10, "0")));
	assert_eq!(parse_radix("0x"), Ok((16, "")));
	assert_eq!(parse_radix("2r101"), Ok((2, "101")));
	assert_eq!(parse_radix("36rzz"), Ok((36, "zz")));
	assert_eq!(parse_radix("1_6rff"), Ok((16, "ff")));
	assert_eq!(parse_radix("16r"), Ok((16, "")));
	assert_eq!(parse_radix("37r1"), Err(RadixTooLarge(big("37"))));
	assert_eq!(parse_radix("99999999999999999999r1"), Err(RadixTooLarge(big("99999999999999999999"))));
	assert_eq!(parse_radix("1r0"), Err(RadixTooSmall(big("1"))));
	assert_eq!(parse_radix("0r0"), Err(RadixTooSmall(big("0"))));
	assert_eq!(parse_radix("16_r1"), Err(NumHasTrailingUnderscore));
	assert_eq!(parse_radix("_1r1"), Err(DigitsHaveLeadingUnderscore));
}

#[test]
fn ints() {
	check(&[
		("0", int("0", None)),
		("42", int("42", None)),
		("007", int("7", None)),
		("1_000_000", int("1000000", None)),
		("1__0", int("10", None)),
		("18446744073709551616", int("18446744073709551616", None)),
		("123456789012345678901234567890123456789", int("123456789012345678901234567890123456789", None)),
		("0b1010", int("10", None)),
		("0q123", int("27", None)),
		("0o777", int("511", None)),
		("0d99", int("99", None)),
		("0xff", int("255", None)),
		("0xFF", int("255", None)),
		("0xdead_beef", int("3735928559", None)),
		("2r101", int("5", None)),
		("16rFF", int("255", None)),
		("36rzz", int("1295", None)),
		("0x1f32", int("7986", None)), // f is a hex digit, not a suffix
		("0x1e3", int("483", None)), // e is a hex digit, not an exponent
	]);
}

#[test]
fn int_errors() {
	check(&[
		("0x", Err(NumHasNoDigits)),
		("2r", Err(NumHasNoDigits)),
		("0x_ff", Err(DigitsHaveLeadingUnderscore)),
		("0b_1", Err(DigitsHaveLeadingUnderscore)),
		("1_", Err(NumHasTrailingUnderscore)),
		("1__", Err(NumHasTrailingUnderscore)),
		("0xff_", Err(NumHasTrailingUnderscore)),
		("0b102", digit('2', 2, 2)),
		("0o8", digit('8', 8, 8)),
		("0x1g", digit('g', 16, 16)),
		("12abc", digit('a', 10, 10)),
		("12q8", digit('q', 26, 10)),
		("2r2", digit('2', 2, 2)),
		("37r1", Err(RadixTooLarge(big("37")))),
		("1r0", Err(RadixTooSmall(big("1")))),
	]);
}

#[test]
fn int_suffixes() {
	check(&[
		("12i64", int("12", Some(Signed(64)))),
		("12u8", int("12", Some(Unsigned(8)))),
		("12i13", int("12", Some(Signed(13)))),
		("1_u8", int("1", Some(Unsigned(8)))),
		("0xffu8", int("255", Some(Unsigned(8)))),
		("0b11u2", int("3", Some(Unsigned(2)))),
		("2r1i3", int("1", Some(Signed(3)))),
		("1u1000000", int("1", Some(Unsigned(1000000)))),
		("1b1", int("1", Some(Bool(1)))),
		("0_b1", int("0", Some(Bool(1)))),
		("1b64", int("1", Some(Bool(64)))),
		("1b7", int("1", Some(Bool(7)))),
		("0b1b1", int("1", Some(Bool(1)))), // binary prefix with a bool suffix
		("0b1", int("1", None)), // binary prefix, not a bool
		("12u", suffix("u")),
		("12i", suffix("i")),
		("12b", suffix("b")),
		("12u8x", suffix("u8x")),
		("12u_8", suffix("u_8")),
		("1b0", suffix("b0")),
		("1u99999999999999999999", suffix("u99999999999999999999")),
	]);
}

#[test]
fn int_suffix_ranges() {
	check(&[
		("255u8", int("255", Some(Unsigned(8)))),
		("256u8", no_fit("256", Unsigned(8))),
		("127u7", int("127", Some(Unsigned(7)))),
		("128u7", no_fit("128", Unsigned(7))),
		("1u1", int("1", Some(Unsigned(1)))),
		("2u1", no_fit("2", Unsigned(1))),
		("0u0", int("0", Some(Unsigned(0)))),
		("1u0", no_fit("1", Unsigned(0))),
		("127i8", int("127", Some(Signed(8)))),
		("128i8", int("128", Some(Signed(8)))), // so `-128i8` works, the sign is its own token
		("129i8", no_fit("129", Signed(8))),
		("255i8", no_fit("255", Signed(8))),
		("1i1", int("1", Some(Signed(1)))),
		("2i1", no_fit("2", Signed(1))),
		("0i0", int("0", Some(Signed(0)))),
		("1i0", no_fit("1", Signed(0))),
		("340282366920938463463374607431768211455u128", int("340282366920938463463374607431768211455", Some(Unsigned(128)))),
		("340282366920938463463374607431768211456u128", no_fit("340282366920938463463374607431768211456", Unsigned(128))),
		("170141183460469231731687303715884105728i128", int("170141183460469231731687303715884105728", Some(Signed(128)))),
		("170141183460469231731687303715884105729i128", no_fit("170141183460469231731687303715884105729", Signed(128))),
		("0b1b8", int("1", Some(Bool(8)))),
		("2b8", no_fit("2", Bool(8))),
	]);
}

#[test]
fn reals() {
	check(&[
		("1.5", real("15", 10, -1, None)),
		("0.25", real("25", 10, -2, None)),
		("1.0", real("10", 10, -1, None)),
		("00.10", real("10", 10, -2, None)),
		("1_000.000_1", real("10000001", 10, -4, None)),
		("3.14159265358979323846264338327950288419716939937510", real("314159265358979323846264338327950288419716939937510", 10, -50, None)),
		("1e3", real("1", 10, 3, None)),
		("1E3", real("1", 10, 3, None)),
		("1e+3", real("1", 10, 3, None)),
		("1e-3", real("1", 10, -3, None)),
		("1.5e3", real("15", 10, 2, None)),
		("1.5e-3", real("15", 10, -4, None)),
		("1e1_0", real("1", 10, 10, None)),
		("0d1.5e1", real("15", 10, 0, None)),
		("12f32", real("12", 10, 0, Some(32))),
		("1.5f64", real("15", 10, -1, Some(64))),
		("1e3f16", real("1", 10, 3, Some(16))),
		("1.5f7", real("15", 10, -1, Some(7))),
		("1_f32", real("1", 10, 0, Some(32))),
		("0x1.8", real("24", 16, -1, None)),
		("0b1.1", real("3", 2, -1, None)),
		("0o0.4", real("4", 8, -1, None)),
		("36rz.z", real("1295", 36, -1, None)),
		("0b1.1f32", real("3", 2, -1, Some(32))),
	]);
}

#[test]
fn real_errors() {
	check(&[
		("1.5u8", Err(RealWithIntSuffix(Unsigned(8)))),
		("1e3i32", Err(RealWithIntSuffix(Signed(32)))),
		("1.5b1", Err(RealWithIntSuffix(Bool(1)))),
		("1.5f0", suffix("f0")),
		("1.5f", suffix("f")),
		("1e", Err(ExponentHasNoDigits)),
		("1e+", Err(ExponentHasNoDigits)),
		("1e-", Err(ExponentHasNoDigits)),
		("1e_3", Err(DigitsHaveLeadingUnderscore)),
		("1e3_", Err(NumHasTrailingUnderscore)),
		("1e99999999999999999999", Err(ExponentTooLarge)),
		("1._5", Err(DigitsHaveLeadingUnderscore)),
		("1.5_", Err(NumHasTrailingUnderscore)),
		("1.", Err(NumHasNoDigits)),
		("0b1.2", digit('2', 2, 2)),
		("0x1.8p3", digit('p', 25, 16)),
		("0b1e1", digit('e', 14, 2)), // only decimal literals have exponents
		("1.5x", digit('x', 33, 10)),
	]);
}

#[test]
fn real_to_f64() {
	let to_f64 = |text| match parse_num(text) {
		Ok(NumLit::Real(lit)) => lit.to_f64(),
		other => panic!("{:?} isn't a real: {:?}", text, other),
	};
	assert_eq!(to_f64("1.5"), 1.5);
	assert_eq!(to_f64("0x1.8"), 1.5);
	assert_eq!(to_f64("0b0.01"), 0.25);
	assert_eq!(to_f64("2e3"), 2000.0);
	assert!((to_f64("1.5e-3") - 0.0015).abs() < 1e-15);
}
