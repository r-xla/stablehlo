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
  expect_snapshot(repr(op_constant(3.14, elt_type = "f64")))
  expect_snapshot(repr(op_constant(3L, elt_type = "i32")))
  expect_snapshot(repr(op_constant(TRUE, elt_type = "pred")))
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
  x_i32 <- hlo_constant(42L, elt_type = "i32")
  expect_s3_class(x_i32@value_type@type@dtype@type, "stablehlo::IntegerType")
  expect_equal(x_i32@value_type@type@dtype@type@Value, "i32")

  x_f64 <- hlo_constant(3.14, elt_type = "f64")
  expect_s3_class(x_f64@value_type@type@dtype@type, "stablehlo::FloatType")
  expect_equal(x_f64@value_type@type@dtype@type@Value, "f64")
})

test_that("Can create a function with boolean constant", {
  local_reset_id_gen()
  x <- hlo_constant(TRUE)
  f <- hlo_return(x)
  expect_s3_class(f, "stablehlo::Func")
})

test_that("scalar constants compile", {
  skip_if_not_installed("pjrt")
  local_reset_id_gen()
  # float
  x <- hlo_constant(-3)
  f <- hlo_return(x)
  f@id@id <- "main"
  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(f)))
  expect_equal(pjrt::pjrt_execute(exec), pjrt::pjrt_scalar(-3))

  # int
  x <- hlo_constant(3L, elt_type = "i32")
  f <- hlo_return(x)
  f@id@id <- "main"
  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(f)))
  expect_equal(pjrt::pjrt_execute(exec), pjrt::pjrt_scalar(3L))

  # unsigned int
  x <- hlo_constant(3L, elt_type = "ui32")
  f <- hlo_return(x)
  f@id@id <- "main"
  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(f)))
  expect_equal(
    pjrt::pjrt_execute(exec),
    pjrt::pjrt_scalar(3L, elt_type = "ui32")
  )
})
