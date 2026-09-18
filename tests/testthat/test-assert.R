test_that("assert_vt_has_ttype", {
  y <- make_vt("i32", integer())

  expect_snapshot(
    assert_vt_has_ttype(x = y, "bool", shape = integer()),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_has_ttype(x = y, "int", shape = 1L),
    error = TRUE
  )
  expect_error(
    assert_vt_has_ttype(x = y, "int", shape = integer()),
    NA
  )

  # Test with initialized dtype instance
  expect_error(
    assert_vt_has_ttype(x = y, as_dtype("i32"), shape = integer()),
    NA
  )
  expect_snapshot(
    assert_vt_has_ttype(x = y, as_dtype("i64"), shape = integer()),
    error = TRUE
  )

  # Test with naxes argument
  scalar <- make_vt("f32", integer())
  vector <- make_vt("f32", 3L)
  matrix <- make_vt("f32", c(2L, 3L))

  # Should pass when naxes matches
  expect_error(
    assert_vt_has_ttype(scalar, naxes = 0L),
    NA
  )
  expect_error(
    assert_vt_has_ttype(vector, naxes = 1L),
    NA
  )
  expect_error(
    assert_vt_has_ttype(matrix, naxes = 2L),
    NA
  )

  # Should fail when naxes doesn't match
  expect_snapshot(
    assert_vt_has_ttype(scalar, naxes = 1L),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_has_ttype(vector, naxes = 2L),
    error = TRUE
  )
  expect_snapshot(
    assert_vt_has_ttype(matrix, naxes = 1L),
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

test_that("assert_const rejects a missing value in the attribute's data", {
  # Every inference function reads `$data` and compares it, so an `NA` that got
  # this far came back out as R's own "missing value where TRUE/FALSE needed".
  na_attr <- cnst(c(NA_integer_, 1L), "i64", 2L)
  expect_error(
    assert_const(na_attr, dtype = as_dtype("i64")),
    "must not contain missing values",
    fixed = TRUE
  )
  expect_error(
    assert_const(cnst(c(0L, 1L), "i64", 2L), dtype = as_dtype("i64")),
    NA
  )
})

test_that("assert_dimvec rejects dimension numbers it cannot compare", {
  expect_error(
    assert_dimvec(c(NA_integer_, 1L)),
    "must be a vector of whole numbers without missing values",
    fixed = TRUE
  )
  expect_error(assert_dimvec("a"), "must be a vector of whole numbers")
  expect_error(
    assert_dimvec(c(1L, 2L), len = 1L),
    "must have 1 entry",
    fixed = TRUE
  )
  expect_identical(assert_dimvec(c(0, 1)), c(0L, 1L))
})

test_that("the inference functions refuse a missing dimension number", {
  x <- make_vt("f32", c(2L, 3L))
  expect_error(
    infer_types_transpose(x, cnst(c(NA_integer_, 1L), "i64", 2L)),
    "must not contain missing values",
    fixed = TRUE
  )
  expect_error(
    infer_types_pad(
      x,
      make_vt("f32", integer()),
      cnst(c(NA_integer_, 0L), "i64", 2L),
      cnst(c(0L, 0L), "i64", 2L),
      cnst(c(0L, 0L), "i64", 2L)
    ),
    "must not contain missing values",
    fixed = TRUE
  )
  expect_error(
    GatherDimensionNumbers(
      offset_dims = c(NA_integer_, 1L),
      collapsed_slice_dims = 0L,
      start_index_map = 0L,
      index_vector_dim = 1L
    ),
    "without missing values",
    fixed = TRUE
  )
})
