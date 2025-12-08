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

test_that("assert_vts_have_same_dtype", {
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

test_that("assert_tensor_constant", {
  expect_error(
    assert_tensor_constant(1L),
    "must be an.*Constant"
  )

  expect_error(
    assert_tensor_constant(NULL, null_ok = TRUE),
    NA
  )
  expect_error(
    assert_tensor_constant(NULL, null_ok = FALSE),
    "must be an.*Constant"
  )

  tensor_const <- Constant(TensorConstant(
    data = 1:6,
    type = TensorType(IntegerType(32L), Shape(c(2L, 3L)))
  ))
  expect_error(
    assert_tensor_constant(tensor_const),
    NA
  )

  expect_error(
    assert_tensor_constant(tensor_const, ndims = 1L),
    "must have.*1.*dimensions"
  )
  expect_error(
    assert_tensor_constant(tensor_const, ndims = 2L),
    NA
  )

  expect_error(
    assert_tensor_constant(tensor_const, dtype = "f32"),
    "must have element type"
  )
  expect_error(
    assert_tensor_constant(tensor_const, dtype = "i32"),
    NA
  )
})

test_that("assert_valid_name", {
  expect_error(assert_valid_name("foo"), NA)
  expect_error(assert_valid_name("Foo123"), NA)
  expect_error(assert_valid_name("a_b_c"), NA)

  expect_error(assert_valid_name("123"), NA)
  expect_error(assert_valid_name("0"), NA)

  expect_error(assert_valid_name("_foo"), "pattern")
  expect_error(assert_valid_name("1abc"), "pattern")
  expect_error(assert_valid_name("foo-bar"), "pattern")
  expect_error(assert_valid_name(""), "pattern")
})

test_that("assert_one_of", {
  x <- vt("i32", integer())

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
