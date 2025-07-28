test_that("op_constant", {
  # Test with a positive value
  op <- op_constant(3.14)
  expect_snapshot(repr(op_constant(3.14)))
  expect_snapshot(repr(op_constant(3L)))
  expect_snapshot(repr(op_constant(-3L)))
  expect_snapshot(repr(op_constant(-100L)))
  expect_snapshot(repr(op_constant(TRUE)))
  expect_snapshot(repr(op_constant(FALSE)))

  # Test with element_type
  expect_snapshot(repr(op_constant(3.14, element_type = "f64")))
  expect_snapshot(repr(op_constant(3L, element_type = "s32")))
  expect_snapshot(repr(op_constant(TRUE, element_type = "pred")))
})

test_that("Can create a function with no inputs", {
  local_reset_id_gen()
  x <- hlo_constant(3.14)
  f <- hlo_return(x)
  expect_snapshot(repr(f))
})

test_that("op_constant works with boolean values", {
  # Test with TRUE
  op_true <- op_constant(TRUE)
  expect_snapshot(repr(op_true))

  # Test with FALSE
  op_false <- op_constant(FALSE)
  expect_snapshot(repr(op_false))
})

test_that("Can create boolean constants with hlo_constant", {
  local_reset_id_gen()

  # Test with TRUE
  x_true <- hlo_constant(TRUE)
  expect_s3_class(x_true@value_type@type@dtype@type, "stablehlo::BooleanType")
  expect_s3_class(x_true, "stablehlo::FuncVariable")

  # Test with FALSE
  x_false <- hlo_constant(FALSE)
  expect_s3_class(x_false@value_type@type@dtype@type, "stablehlo::BooleanType")
  expect_s3_class(x_false, "stablehlo::FuncVariable")
})

test_that("Can create integer constants with hlo_constant", {
  local_reset_id_gen()

  # Test with positive integer
  x_pos <- hlo_constant(42L)
  expect_s3_class(x_pos@value_type@type@dtype@type, "stablehlo::IntegerType")
  expect_s3_class(x_pos, "stablehlo::FuncVariable")

  # Test with negative integer
  x_neg <- hlo_constant(-123L)
  expect_s3_class(x_neg@value_type@type@dtype@type, "stablehlo::IntegerType")
  expect_s3_class(x_neg, "stablehlo::FuncVariable")

  # Test with element_type
  x_s32 <- hlo_constant(42L, element_type = "s32")
  expect_s3_class(x_s32@value_type@type@dtype@type, "stablehlo::IntegerType")
  expect_equal(x_s32@value_type@type@dtype@type@Value, "si32")

  x_f64 <- hlo_constant(3.14, element_type = "f64")
  expect_s3_class(x_f64@value_type@type@dtype@type, "stablehlo::FloatType")
  expect_equal(x_f64@value_type@type@dtype@type@Value, "f64")
})

test_that("Can create a function with boolean constant", {
  local_reset_id_gen()
  x <- hlo_constant(TRUE)
  f <- hlo_return(x)
  expect_s3_class(f, "stablehlo::Func")
})
