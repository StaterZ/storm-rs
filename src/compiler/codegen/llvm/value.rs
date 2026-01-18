use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use inkwell::{
	llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef}, module::Linkage, types::{AnyType, ArrayType, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, FunctionType, IntType, PointerType, StructType}, values::{AnyValue, ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PhiValue, PointerValue, StructValue}
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PtrValue<'ctx> {
	value: PointerValue<'ctx>,
	pointee_type: ValueType<'ctx>,
}

impl<'ctx> PtrValue<'ctx> {
	pub fn new(value: PointerValue<'ctx>, pointee_type: ValueType<'ctx>) -> Self {
		Self {
			value,
			pointee_type,
		}
	}

	pub fn get_type(self) -> PtrValueType<'ctx> {
		PtrValueType {
			value: self.value.get_type(),
			pointee: self.pointee_type,
		}
	}
	
	pub fn get_pointee_type(&self) -> &ValueType<'ctx> {
		&self.pointee_type
	}
}
impl<'ctx> Deref for PtrValue<'ctx> {
	type Target = PointerValue<'ctx>;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}
unsafe impl<'ctx> AsValueRef for PtrValue<'ctx> {
	fn as_value_ref(&self) -> LLVMValueRef {
		self.value.as_value_ref()
	}
}
unsafe impl<'ctx> AnyValue<'ctx> for PtrValue<'ctx> {
}
unsafe impl<'ctx> BasicValue<'ctx> for PtrValue<'ctx> {
}

#[derive(Debug, PartialEq, Eq, Clone, EnumAsInner)]
pub enum Value<'ctx> {
	Ptr(PtrValue<'ctx>),
	Int(IntValue<'ctx>),
	Flt(FloatValue<'ctx>),
	Phi(PhiValue<'ctx>),
	Func(FunctionValue<'ctx>),
	Struct(StructValue<'ctx>),
	Array(ArrayValue<'ctx>),
}

impl<'ctx> Value<'ctx> {
	pub fn new(value: BasicValueEnum<'ctx>, r#type: ValueType<'ctx>) -> Self {
		match (value, r#type) {
			(BasicValueEnum::ArrayValue(value), ValueType::Array(_type)) => Self::Array(value),
			(BasicValueEnum::IntValue(value), ValueType::Int(_type)) => Self::Int(value),
			(BasicValueEnum::FloatValue(value), ValueType::Flt(_type)) => Self::Flt(value),
			(BasicValueEnum::PointerValue(value), ValueType::Ptr(box r#type)) => Self::Ptr(PtrValue::new(value, r#type.pointee)),
			(BasicValueEnum::StructValue(value), ValueType::Struct(_type)) => Self::Struct(value),
			_ => unreachable!("type is not correct for value"),
		}
	}

	pub fn get_type(self) -> ValueType<'ctx> {
		match self {
			Value::Ptr(value) => ValueType::Ptr(Box::new(value.get_type())),
			Value::Int(value) => ValueType::Int(value.get_type()),
			Value::Flt(value) => ValueType::Flt(value.get_type()),
			Value::Phi(_value) => todo!(),
			Value::Func(value) => ValueType::Func(FuncType::new(value)),
			Value::Struct(value) => ValueType::Struct(value.get_type()),
			Value::Array(value) => ValueType::Array(value.get_type()),
		}
	}
}

impl<'ctx> From<BasicValueEnum<'ctx>> for Value<'ctx> {
	fn from(value: BasicValueEnum<'ctx>) -> Self {
		match value {
			BasicValueEnum::ArrayValue(value) => Self::Array(value),
			BasicValueEnum::IntValue(value) => Self::Int(value),
			BasicValueEnum::FloatValue(value) => Self::Flt(value),
			BasicValueEnum::PointerValue(_value) => todo!(),
			BasicValueEnum::StructValue(value) => Self::Struct(value),
			BasicValueEnum::VectorValue(_value) => todo!(),
			BasicValueEnum::ScalableVectorValue(_value) => todo!(),
		}
	}
}
impl<'ctx> Into<BasicValueEnum<'ctx>> for Value<'ctx> {
	fn into(self) -> BasicValueEnum<'ctx> {
		match self {
			Value::Ptr(value) => BasicValueEnum::PointerValue(value.value),
			Value::Int(value) => BasicValueEnum::IntValue(value),
			Value::Flt(value) => BasicValueEnum::FloatValue(value),
			Value::Phi(_value) => todo!(),
			Value::Func(_value) => todo!(),
			Value::Struct(value) => BasicValueEnum::StructValue(value),
			Value::Array(value) => BasicValueEnum::ArrayValue(value),
		}
	}
}
impl<'ctx> Into<BasicMetadataValueEnum<'ctx>> for Value<'ctx> {
	fn into(self) -> BasicMetadataValueEnum<'ctx> {
		match self {
			Value::Ptr(value) => BasicMetadataValueEnum::PointerValue(value.value),
			Value::Int(value) => BasicMetadataValueEnum::IntValue(value),
			Value::Flt(value) => BasicMetadataValueEnum::FloatValue(value),
			Value::Phi(_value) => todo!(),
			Value::Func(_value) => todo!(),
			Value::Struct(value) => BasicMetadataValueEnum::StructValue(value),
			Value::Array(value) => BasicMetadataValueEnum::ArrayValue(value),
		}
	}
}

unsafe impl<'ctx> AsValueRef for Value<'ctx> {
	fn as_value_ref(&self) -> LLVMValueRef {
		match self {
			Value::Ptr(value) => value.as_value_ref(),
			Value::Int(value) => value.as_value_ref(),
			Value::Flt(value) => value.as_value_ref(),
			Value::Phi(value) => value.as_value_ref(),
			Value::Func(value) => value.as_value_ref(),
			Value::Struct(value) => value.as_value_ref(),
			Value::Array(value) => value.as_value_ref(),
		}
	}
}
unsafe impl<'ctx> AnyValue<'ctx> for Value<'ctx> {
}
unsafe impl<'ctx> BasicValue<'ctx> for Value<'ctx> {
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PtrValueType<'ctx> {
	value: PointerType<'ctx>,
	pointee: ValueType<'ctx>,
}

unsafe impl<'ctx> AsTypeRef for PtrValueType<'ctx> {
	fn as_type_ref(&self) -> LLVMTypeRef {
		self.value.as_type_ref()
	}
}
unsafe impl<'ctx> AnyType<'ctx> for PtrValueType<'ctx> {
}
unsafe impl<'ctx> BasicType<'ctx> for PtrValueType<'ctx> {
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncType<'ctx> {
	value: FunctionType<'ctx>,
	linkage: Linkage,
}
impl<'ctx> FuncType<'ctx> {
	pub fn new(func: FunctionValue<'ctx>) -> Self {
		Self {
			value: func.get_type(),
			linkage: func.get_linkage(),
		}
	}
	
	pub fn get_linkage(&self) -> Linkage {
		self.linkage
	}
}

impl<'ctx> Into<FunctionType<'ctx>> for FuncType<'ctx> {
	fn into(self) -> FunctionType<'ctx> {
		self.value
	}
}

unsafe impl<'ctx> AsTypeRef for FuncType<'ctx> {
	fn as_type_ref(&self) -> LLVMTypeRef {
		self.value.as_type_ref()
	}
}
unsafe impl<'ctx> AnyType<'ctx> for FuncType<'ctx> {
}
unsafe impl<'ctx> BasicType<'ctx> for FuncType<'ctx> {
}

#[derive(Debug, PartialEq, Eq, Clone, EnumAsInner)]
pub enum ValueType<'ctx> {
	Ptr(Box<PtrValueType<'ctx>>),
	Int(IntType<'ctx>),
	Flt(FloatType<'ctx>),
	Func(FuncType<'ctx>),
	Struct(StructType<'ctx>),
	Array(ArrayType<'ctx>),
}

impl<'ctx> From<BasicTypeEnum<'ctx>> for ValueType<'ctx> {
	fn from(value: BasicTypeEnum<'ctx>) -> Self {
		match value {
			BasicTypeEnum::ArrayType(value) => Self::Array(value),
			BasicTypeEnum::IntType(value) => Self::Int(value),
			BasicTypeEnum::FloatType(value) => Self::Flt(value),
			BasicTypeEnum::PointerType(_value) => todo!(),
			BasicTypeEnum::StructType(value) => Self::Struct(value),
			BasicTypeEnum::VectorType(_value) => todo!(),
			BasicTypeEnum::ScalableVectorType(_value) => todo!(),
		}
	}
}
impl<'ctx> Into<BasicTypeEnum<'ctx>> for ValueType<'ctx> {
	fn into(self) -> BasicTypeEnum<'ctx> {
		match self {
			ValueType::Ptr(value) => BasicTypeEnum::PointerType(value.value),
			ValueType::Int(value) => BasicTypeEnum::IntType(value),
			ValueType::Flt(value) => BasicTypeEnum::FloatType(value),
			ValueType::Func(_value) => todo!(),
			ValueType::Struct(value) => BasicTypeEnum::StructType(value),
			ValueType::Array(value) => BasicTypeEnum::ArrayType(value),
		}
	}
}
impl<'ctx> Into<BasicMetadataTypeEnum<'ctx>> for ValueType<'ctx> {
	fn into(self) -> BasicMetadataTypeEnum<'ctx> {
		match self {
			ValueType::Ptr(value) => BasicMetadataTypeEnum::PointerType(value.value),
			ValueType::Int(value) => BasicMetadataTypeEnum::IntType(value),
			ValueType::Flt(value) => BasicMetadataTypeEnum::FloatType(value),
			ValueType::Func(_value) => todo!(),
			ValueType::Struct(value) => BasicMetadataTypeEnum::StructType(value),
			ValueType::Array(value) => BasicMetadataTypeEnum::ArrayType(value),
		}
	}
}

unsafe impl<'ctx> AsTypeRef for ValueType<'ctx> {
	fn as_type_ref(&self) -> LLVMTypeRef {
		match self {
			ValueType::Ptr(value) => value.as_type_ref(),
			ValueType::Int(value) => value.as_type_ref(),
			ValueType::Flt(value) => value.as_type_ref(),
			ValueType::Func(value) => value.as_type_ref(),
			ValueType::Struct(value) => value.as_type_ref(),
			ValueType::Array(value) => value.as_type_ref(),
		}
	}
}
unsafe impl<'ctx> AnyType<'ctx> for ValueType<'ctx> {
}
unsafe impl<'ctx> BasicType<'ctx> for ValueType<'ctx> {
}
