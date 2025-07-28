test_that("Constant TensorConstant", {
  const <- TensorConstant(
    data = 1.0,
    type = TensorType(
      dtype = TensorElementType(type = FloatType("f32")),
      shape = Shape()
    )
  )

  expect_snapshot(
    repr(const)
  )
})

test_that("BooleanConstant works correctly", {
  true_constant <- BooleanConstant(TRUE)
  false_constant <- BooleanConstant(FALSE)

  expect_snapshot(repr(true_constant))
  expect_snapshot(repr(false_constant))
})

test_that("IntegerConstant works correctly", {
  int_constant <- IntegerConstant(value = 42L, type = IntegerType("si64"))
  negative_int <- IntegerConstant(value = -123L, type = IntegerType("si32"))

  expect_snapshot(repr(int_constant))
  expect_snapshot(repr(negative_int))
})

test_that("FloatConstant works correctly", {
  f <- FloatLiteral(
    sign_part = SignPart("+"),
    integer_part = IntegerPart(list(decimalDigit(3L), decimalDigit(1L), decimalDigit(4L))),
    fractional_part = FractionalPart(list(decimalDigit(1L), decimalDigit(5L), decimalDigit(9L))),
    scientific_part = ScientificPart(
      exponent_sign = SignPart("+"),
      exponent_digits = IntegerPart(list(decimalDigit(0)))
    )
  )
  float_constant <- FloatConstant(literal = f, type = FloatType("f32"))
  expect_snapshot(repr(float_constant))
})

test_that("TensorConstant works correctly", {
  # Test with numeric data
  tensor_const <- TensorConstant(
    data = 3.14,
    type = TensorType(
      dtype = TensorElementType(type = FloatType("f32")),
      shape = Shape()
    )
  )
  expect_snapshot(repr(tensor_const))

  # Test with logical data
  bool_tensor <- TensorConstant(
    data = TRUE,
    type = TensorType(
      dtype = TensorElementType(type = BooleanType()),
      shape = Shape()
    )
  )
  expect_snapshot(repr(bool_tensor))

  # Test with integer data
  int_tensor <- TensorConstant(
    data = 42L,
    type = TensorType(
      dtype = TensorElementType(type = IntegerType("si64")),
      shape = Shape()
    )
  )
  expect_snapshot(repr(int_tensor))
})
