test_that("assert_vt_has_ttype", {
  y <- vt("i32", integer())

  expect_error(
    assert_vt_has_ttype(x = 1),
    "must be a ValueType"
  )
  expect_error(
    assert_vt_has_ttype(x = y, BooleanType, shape = integer()),
    "must be one of"
  )
  expect_error(
    assert_vt_has_ttype(x = y, IntegerType, shape = 1L),
    "Got ()"
  )
  expect_error(
    assert_vt_has_ttype(x = y, IntegerType, shape = integer()),
    NA
  )
})

test_that("assert_vt_is_tensor", {
  y <- 1L
  expect_error(
    assert_vt_is_tensor(x = y),
    "must be a ValueType"
  )

  token <- ValueType(TokenType())
  expect_error(
    assert_vt_is_tensor(x = token),
    "must contain a TensorType"
  )

  z <- vt("i32", integer())
  expect_error(
    assert_vt_is_tensor(x = z),
    NA
  )
})

test_that("assert_vts_are_tensors", {
  x <- vt("i32", integer())
  token <- ValueType(TokenType())
  expect_error(
    assert_vts_are_tensors(x, 1L),
    "must be a ValueType"
  )
  expect_error(
    assert_vts_are_tensors(x = token),
    "must contain a TensorType"
  )
  expect_error(
    assert_vts_are_tensors(x),
    NA
  )
})

test_that("assert_vt_equal", {
  x <- vt("i32", integer())
  y <- vt("i32", integer())
  z1 <- vt("i32", 1L)
  z2 <- vt("f32", integer())

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

test_that("assert_vt_has_same_dtype", {
  x <- vt("i32", integer())
  y <- vt("f32", integer())
  z <- vt("i32", 1L)
  expect_error(
    assert_vts_have_same_dtype(x, y),
    "must have the same dtype"
  )
  expect_error(
    assert_vts_have_same_dtype(x, z),
    NA
  )
})
