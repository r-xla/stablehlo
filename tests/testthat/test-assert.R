test_that("assert_vt_is_tensor", {
  y <- make_vt("i32", integer())

  expect_snapshot(
    assert_vt_is_tensor(x = 1),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(BooleanType), expected_shape = integer()),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType), expected_shape = 1L),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType), expected_shape = integer()),
    error = FALSE
  )

  # Test with instantiated dtypes
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType(32))),
    error = FALSE
  )
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType(64))),
    error = TRUE
  )

  # Test mixing classes and instances
  expect_snapshot(
    assert_vt_is_tensor(x = y, expected_dtypes = list(BooleanType, IntegerType(32))),
    error = FALSE
  )
})

test_that("assert_vt_is_tensor", {
  y <- 1L
  expect_snapshot(
    assert_vt_is_tensor(x = y),
    error = TRUE
  )

  token <- ValueType(TokenType())
  expect_snapshot(
    assert_vt_is_tensor(x = token),
    error = TRUE
  )

  z <- make_vt("i32", integer())
  expect_snapshot(
    assert_vt_is_tensor(x = z),
    error = FALSE
  )
})

test_that("assert_vts_are_tensors", {
  x <- make_vt("i32", integer())
  token <- ValueType(TokenType())
  expect_snapshot(
    assert_vts_are_tensors(x, 1L),
    error = TRUE
  )
  expect_snapshot(
    assert_vts_are_tensors(x = token),
    error = TRUE
  )
  expect_error(
    assert_vts_are_tensors(x),
    NA
  )
})

test_that("assert_vt_equal", {
  x <- make_vt("i32", integer())
  y <- make_vt("i32", integer())
  z1 <- make_vt("i32", 1L)
  z2 <- make_vt("f32", integer())

  expect_error(
    assert_vt_equal(x, z1),
    "to be equal"
  )
  expect_error(
    assert_vt_equal(x, z2),
    "to be equal"
  )

  expect_error(
    assert_vt_equal(x, y),
    NA
  )
})

test_that("assert_vts_have_same_dtype", {
  x <- make_vt("i32", integer())
  y <- make_vt("f32", integer())
  z <- make_vt("i32", 1L)
  expect_error(
    assert_vts_have_same_dtype(x, y),
    "must have the same dtype"
  )
  expect_error(
    assert_vts_have_same_dtype(x, z),
    NA
  )
})

test_that("assert_valid_id", {
  expect_error(assert_valid_id("foo"), NA)
  expect_error(assert_valid_id("Foo123"), NA)
  expect_error(assert_valid_id("a_b_c"), NA)

  expect_error(assert_valid_id("123"), NA)
  expect_error(assert_valid_id("0"), NA)

  expect_error(assert_valid_id("_foo"), "pattern")
  expect_error(assert_valid_id("1abc"), "pattern")
  expect_error(assert_valid_id("foo-bar"), "pattern")
  expect_error(assert_valid_id(""), "pattern")
})

test_that("assert_one_of", {
  x <- make_vt("i32", integer())

  expect_error(
    assert_one_of(x, ValueType),
    NA
  )

  expect_error(
    assert_one_of(x, TensorType, TokenType),
    "must be a"
  )

  expect_error(
    assert_one_of(x, TensorType, ValueType),
    NA
  )
})
