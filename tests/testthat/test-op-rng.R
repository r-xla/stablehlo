test_that("rng uniform", {
  func <- local_func()
  a <- hlo_scalar(0.0, dtype = "f32")
  b <- hlo_scalar(1.0, dtype = "f32")
  shape <- hlo_tensor(c(3L, 3L), dtype = "i64", shape = 2L)
  result <- hlo_rng(a, b, shape, "UNIFORM")
  expect_snapshot(repr(result@func@body@items[[4]]))
})

test_that("rng normal", {
  func <- local_func()
  a <- hlo_scalar(0.0, dtype = "f32")
  b <- hlo_scalar(1.0, dtype = "f32")
  shape <- hlo_tensor(c(2L, 2L), dtype = "i64", shape = 2L)
  result <- hlo_rng(a, b, shape, "NORMAL")
  expect_snapshot(repr(result@func@body@items[[4]]))
})

test_that("rng with integer types", {
  func <- local_func()
  a <- hlo_scalar(0L, dtype = "i32")
  b <- hlo_scalar(10L, dtype = "i32")
  shape <- hlo_tensor(c(2L, 2L), dtype = "i64", shape = 2L)
  result <- hlo_rng(a, b, shape, "UNIFORM")
  expect_snapshot(repr(result@func@body@items[[4]]))
})

test_that("rng error handling", {
  func <- local_func()
  a <- hlo_scalar(0.0, dtype = "f32")
  b <- hlo_scalar(1.0, dtype = "f32")
  shape <- hlo_tensor(c(3L, 3L), dtype = "i64", shape = 2L)

  # Invalid distribution
  expect_error(
    hlo_rng(a, b, shape, "INVALID"),
    "rng_distribution must be either 'UNIFORM' or 'NORMAL'"
  )

  # Type mismatch between a and b
  b_int <- hlo_scalar(1L, dtype = "i32")
  expect_error(
    hlo_rng(a, b_int, shape, "UNIFORM"),
    "Element types of 'a' and 'b' must be the same"
  )

  # Non-scalar a
  a_tensor <- hlo_tensor(c(0.0, 1.0), dtype = "f32", shape = 2L)
  expect_error(
    hlo_rng(a_tensor, b, shape, "UNIFORM"),
    "'a' must be a 0-dimensional tensor \\(scalar\\)"
  )

  # Non-scalar b
  b_tensor <- hlo_tensor(c(1.0, 2.0), dtype = "f32", shape = 2L)
  expect_error(
    hlo_rng(a, b_tensor, shape, "UNIFORM"),
    "'b' must be a 0-dimensional tensor \\(scalar\\)"
  )

  # Invalid shape tensor
  invalid_shape <- hlo_tensor(c(3L, 3L), dtype = "i32", shape = 2L)
  expect_error(
    hlo_rng(a, b, invalid_shape, "UNIFORM"),
    "'shape' must be of type si64"
  )

  # Non-1D shape tensor
  invalid_shape_2d <- hlo_tensor(array(c(3L, 3L), dim = c(1, 2)), dtype = "i64", shape = c(1L, 2L))
  expect_error(
    hlo_rng(a, b, invalid_shape_2d, "UNIFORM"),
    "'shape' must be a 1-dimensional tensor"
  )

  # NORMAL distribution with non-float types
  a_int <- hlo_scalar(0L, dtype = "i32")
  b_int <- hlo_scalar(1L, dtype = "i32")
  expect_error(
    hlo_rng(a_int, b_int, shape, "NORMAL"),
    "For NORMAL distribution, 'a' and 'b' must be floating-point types"
  )
})
