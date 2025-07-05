test_that("Constant TensorLiteral", {
  f <- FloatLiteral(
    sign_part = SignPart("+"),
    integer_part = IntegerPart(list(decimalDigit(1L))),
    fractional_part = FractionalPart(list(decimalDigit(0))),
    scientific_part = ScientificPart(
      exponent_sign = SignPart("+"),
      exponent_digits = IntegerPart(list(decimalDigit(0)))
    )
  )

  expect_snapshot(
    repr(f)
  )

  const <- Constant(TensorConstant(
    TensorLiteral(
      ElementLiteral(f)
    ),
    TensorType(
      dtype = TensorElementType(type = FloatType("f32")),
      shape = Shape()
    )
  ))

  expect_snapshot(
    repr(const)
  )
})
