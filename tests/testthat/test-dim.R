N <- NA_integer_

test_that("may() is false only when the relation is known false", {
  expect_equal(may(c(TRUE, FALSE, NA)), c(TRUE, FALSE, TRUE))
  expect_equal(must(c(TRUE, FALSE, NA)), c(TRUE, FALSE, FALSE))
})

test_that("the truth table", {
  # a, b, may_eq, may_ge, must_eq
  tbl <- list(
    list(3L, 3L, TRUE, TRUE, TRUE),
    list(3L, 4L, FALSE, FALSE, FALSE),
    list(4L, 3L, FALSE, TRUE, FALSE),
    list(N, 3L, TRUE, TRUE, FALSE),
    list(3L, N, TRUE, TRUE, FALSE),
    list(N, N, TRUE, TRUE, FALSE)
  )
  for (row in tbl) {
    info <- sprintf("a=%s b=%s", row[[1L]], row[[2L]])
    expect_equal(may_eq(row[[1L]], row[[2L]]), row[[3L]], info = info)
    expect_equal(may_ge(row[[1L]], row[[2L]]), row[[4L]], info = info)
    expect_equal(must(row[[1L]] == row[[2L]]), row[[5L]], info = info)
  }
})

test_that("dim_meet refines toward the known side", {
  expect_equal(dim_meet(c(N, 3L), c(2L, N)), c(2L, 3L))
  expect_equal(dim_meet(c(N, N), c(N, N)), c(N, N))
  expect_equal(dim_meet(c(2L, 3L), c(2L, 3L)), c(2L, 3L))
  expect_equal(dim_meet(integer(), integer()), integer())
})

test_that("dim_meet errors only on a definite clash", {
  expect_error(dim_meet(c(2L, 3L), c(2L, 4L)), "dimension")
  expect_no_error(dim_meet(c(2L, N), c(2L, 4L)))
  expect_error(dim_meet(c(2L, 3L), 2L), "same rank")
})

test_that("shapes_meet folds, so a set that cannot agree is caught", {
  # Each shape may match the first, but the second and third cannot possibly
  # match each other -- the reason a pairwise `may_eq` check is unsound.
  expect_error(shapes_meet(list(N, 3L, 4L)), "dimension")
  expect_equal(shapes_meet(list(N, 3L, N)), 3L)
  expect_equal(shapes_meet(list(c(N, 2L), c(5L, N))), c(5L, 2L))
})

test_that("assert_shapevec stays strict, assert_shapevec_dyn does not", {
  expect_error(assert_shapevec(c(2L, N)))
  expect_silent(assert_shapevec_dyn(c(2L, N)))
  expect_error(assert_shapevec_dyn(c(2L, -1L)))
})

test_that("vt_meet refines a tensor type", {
  dyn <- ValueType(TensorType(as_dtype("f32"), Shape(c(N, 3L))))
  sta <- ValueType(TensorType(as_dtype("f32"), Shape(c(2L, N))))
  expect_equal(repr(vt_meet(dyn, sta)), "tensor<2x3xf32>")
  expect_equal(repr(vt_meet(dyn, dyn)), "tensor<?x3xf32>")
})

test_that("vt_meet still rejects a dtype, rank or definite size mismatch", {
  a <- ValueType(TensorType(as_dtype("f32"), Shape(N)))
  b <- ValueType(TensorType(as_dtype("f64"), Shape(N)))
  c2 <- ValueType(TensorType(as_dtype("f32"), Shape(c(N, N))))
  d <- ValueType(TensorType(as_dtype("f32"), Shape(3L)))
  e <- ValueType(TensorType(as_dtype("f32"), Shape(4L)))
  # A two-operand op reports the whole type, which says more than an axis
  # index; `dim_meet()`'s per-axis message is for the fold sites.
  expect_error(vt_meet(a, b), "same tensor type")
  expect_error(vt_meet(a, c2), "same tensor type")
  expect_error(vt_meet(d, e), "same tensor type")
})

test_that("type identity is NOT satisfiability", {
  # The relation buffer aliasing is decided with: `?` must not match a known
  # size, or two buffers of different length would be aliased.
  dyn <- TensorType(as_dtype("f32"), Shape(N))
  sta <- TensorType(as_dtype("f32"), Shape(3L))
  expect_true(dyn == TensorType(as_dtype("f32"), Shape(N)))
  expect_false(dyn == sta)
})
