test_that("possibly() is false only when the relation is known false", {
  expect_equal(possibly(c(TRUE, FALSE, NA)), c(TRUE, FALSE, TRUE))
  expect_equal(provably(c(TRUE, FALSE, NA)), c(TRUE, FALSE, FALSE))
})

test_that("the truth table", {
  # a, b, possibly(a == b), possibly_ge, provably_eq
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
    expect_equal(possibly(row[[1L]] == row[[2L]]), row[[3L]], info = info)
    expect_equal(possibly_ge(row[[1L]], row[[2L]]), row[[4L]], info = info)
    expect_equal(provably_eq(row[[1L]], row[[2L]]), row[[5L]], info = info)
  }
})

test_that("provably_ne is not the complement of possibly-equal", {
  # The distinction the whole design rests on: for `?` against `3`, both
  # "possibly equal" and "possibly unequal" are true, so a check that reads
  # "if these differ, error" has to be `provably_ne` and never a negated
  # "possibly equal". The duality runs the other way: `possibly(a == b)` is
  # the negation of `provably_ne(a, b)`.
  expect_true(possibly(N == 3L))
  expect_false(provably_ne(N, 3L))
  expect_false(possibly(3L == 4L))
  expect_true(provably_ne(3L, 4L))
  expect_false(provably_ne(3L, 3L))
  expect_false(provably_ne(N, N))

  # `possibly(x)` is `!provably(!x)` -- the duality the names are chosen for,
  # over every pairing of a known and an unknown size.
  for (a in c(3L, 4L, N)) {
    for (b in c(3L, 4L, N)) {
      info <- sprintf("a=%s b=%s", a, b)
      expect_equal(possibly(a == b), !provably_ne(a, b), info = info)
      expect_equal(possibly(a >= b), !provably_gt(b, a), info = info)
    }
  }
})

test_that("provably_gt", {
  expect_true(provably_gt(4L, 3L))
  expect_false(provably_gt(3L, 4L))
  expect_false(provably_gt(3L, 3L))
  expect_false(provably_gt(N, 3L))
  expect_false(provably_gt(3L, N))
})

test_that("shape_nelts and provably_nelts_ne", {
  expect_equal(shape_nelts(c(2L, 3L)), 6L)
  # A scalar's element count is 1, which is what reshape between
  # `tensor<1xf32>` and `tensor<f32>` relies on.
  expect_equal(shape_nelts(integer()), 1L)
  expect_true(is.na(shape_nelts(c(2L, N))))
  expect_true(provably_nelts_ne(c(2L, 3L), c(4L, 2L)))
  expect_false(provably_nelts_ne(c(2L, 3L), c(3L, 2L)))
  # Unknown on either side defers.
  expect_false(provably_nelts_ne(c(2L, N), c(3L, 2L)))
  expect_false(provably_nelts_ne(c(2L, 3L), c(N, 2L)))
})

test_that("a known 0 annihilates, so the element count stays decidable", {
  # `prod(c(NA, 0L))` is `NA`, but a shape holding a 0 has 0 elements whatever
  # the `?` turns out to be.
  expect_equal(shape_nelts(c(N, 0L)), 0L)
  expect_equal(shape_nelts(c(0L, N)), 0L)
  expect_true(provably_nelts_ne(c(N, 0L), 5L))
  expect_true(provably_nelts_ne(c(N, 0L), integer()))
  # Without a 0 the count is genuinely unknown and nothing is decided.
  expect_true(is.na(shape_nelts(c(N, 3L))))
  expect_false(provably_nelts_ne(c(N, 3L), 6L))
})

test_that("shape_meet refines toward the known side", {
  expect_equal(shape_meet(c(N, 3L), c(2L, N)), c(2L, 3L))
  expect_equal(shape_meet(c(N, N), c(N, N)), c(N, N))
  expect_equal(shape_meet(c(2L, 3L), c(2L, 3L)), c(2L, 3L))
  expect_equal(shape_meet(integer(), integer()), integer())
})

test_that("shape_meet errors only on a definite clash", {
  expect_error(shape_meet(c(2L, 3L), c(2L, 4L)), "dimension")
  expect_no_error(shape_meet(c(2L, N), c(2L, 4L)))
  expect_error(shape_meet(c(2L, 3L), 2L), "same rank")
})

test_that("shapes_meet folds, so a set that cannot agree is caught", {
  # Each shape may match the first, but the second and third cannot possibly
  # match each other -- the reason a pairwise `possibly_eq` check is unsound.
  expect_error(shapes_meet(list(N, 3L, 4L)), "dimension")
  expect_equal(shapes_meet(list(N, 3L, N)), 3L)
  expect_equal(shapes_meet(list(c(N, 2L), c(5L, N))), c(5L, 2L))
})

test_that("assert_shapevec stays strict, assert_shapevec_dyn does not", {
  expect_error(assert_shapevec(c(2L, N)), "Contains missing values")
  expect_silent(assert_shapevec_dyn(c(2L, N)))
  expect_error(assert_shapevec_dyn(c(2L, -1L)), "is not >= 0")
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
  # index; `shape_meet()`'s per-axis message is for the fold sites.
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
