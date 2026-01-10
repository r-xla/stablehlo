test_that("assert_vt_has_ttype", {
  y <- make_vt("i32", integer())

  expect_snapshot(
    assert_vt_has_ttype(x = y, "BooleanType", shape = integer()),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_has_ttype(x = y, "IntegerType", shape = 1L),
    error = TRUE
  )
  expect_error(
    assert_vt_has_ttype(x = y, "IntegerType", shape = integer()),
    NA
  )

  # Test with initialized dtype instance
  expect_error(
    assert_vt_has_ttype(x = y, IntegerType(32L), shape = integer()),
    NA
  )
  expect_snapshot(
    assert_vt_has_ttype(x = y, IntegerType(64L), shape = integer()),
    error = TRUE
  )
})

test_that("assert_vt_is_tensor", {
  y <- make_vt("i32", integer())

  expect_snapshot(
    assert_vt_is_tensor(x = 1),
    error = TRUE
  )

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

  expect_snapshot(
    assert_vt_equal(x, z1),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_equal(x, z2),
    error = TRUE
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
  expect_snapshot(
    assert_vts_have_same_dtype(x, y),
    error = TRUE
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

  expect_snapshot(assert_valid_id("_foo"), error = TRUE)
  expect_snapshot(assert_valid_id("1abc"), error = TRUE)
  expect_snapshot(assert_valid_id("foo-bar"), error = TRUE)
  expect_snapshot(assert_valid_id(""), error = TRUE)
})

test_that("assert_one_of", {
  x <- make_vt("i32", integer())

  expect_error(
    assert_one_of(x, c("ValueType")),
    NA
  )

  expect_snapshot(
    assert_one_of(x, c("TensorType", "TokenType")),
    error = TRUE
  )

  expect_error(
    assert_one_of(x, c("TensorType", "ValueType")),
    NA
  )
})
