# TensorDataType Base Class

`TensorDataType` is the parent S3 class for all tensor data types. All
data type classes inherit from `TensorDataType`, enabling cross-type
comparisons with `==` and `!=` operators.

The specific data type classes are:

- [`BooleanType()`](https://r-xla.github.io/stablehlo/dev/reference/BooleanType.md) -
  Boolean (i1)

- [`IntegerType()`](https://r-xla.github.io/stablehlo/dev/reference/IntegerType.md) -
  Signed integers (i8, i16, i32, i64)

- [`UnsignedType()`](https://r-xla.github.io/stablehlo/dev/reference/UnsignedType.md) -
  Unsigned integers (ui8, ui16, ui32, ui64)

- [`FloatType()`](https://r-xla.github.io/stablehlo/dev/reference/FloatType.md) -
  Floating point (f32, f64)

## Details

This is a virtual base class - you cannot create instances directly. Use
the specific type constructors instead.

## See also

[`BooleanType()`](https://r-xla.github.io/stablehlo/dev/reference/BooleanType.md),
[`IntegerType()`](https://r-xla.github.io/stablehlo/dev/reference/IntegerType.md),
[`UnsignedType()`](https://r-xla.github.io/stablehlo/dev/reference/UnsignedType.md),
[`FloatType()`](https://r-xla.github.io/stablehlo/dev/reference/FloatType.md)
