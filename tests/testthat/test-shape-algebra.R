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
    expect_equal(may(row[[1L]] == row[[2L]]), row[[3L]], info = info)
    expect_equal(may_ge(row[[1L]], row[[2L]]), row[[4L]], info = info)
    expect_equal(must_eq(row[[1L]], row[[2L]]), row[[5L]], info = info)
  }
})

test_that("must_ne is not the complement of may_eq", {
  # The distinction the whole design rests on: for `?` against `3`, both "may
  # be equal" and "may differ" are true, so a check that reads "if these
  # differ, error" must use must_ne, never !may_eq.
  expect_true(may(N == 3L))
  expect_false(must_ne(N, 3L))
  expect_false(may(3L == 4L))
  expect_true(must_ne(3L, 4L))
  expect_false(must_ne(3L, 3L))
  expect_false(must_ne(N, N))
})

test_that("must_gt", {
  expect_true(must_gt(4L, 3L))
  expect_false(must_gt(3L, 4L))
  expect_false(must_gt(3L, 3L))
  expect_false(must_gt(N, 3L))
  expect_false(must_gt(3L, N))
})

test_that("shape_nelts and must_nelts_ne", {
  expect_equal(shape_nelts(c(2L, 3L)), 6L)
  # A scalar's element count is 1, which is what reshape between
  # `tensor<1xf32>` and `tensor<f32>` relies on.
  expect_equal(shape_nelts(integer()), 1L)
  expect_true(is.na(shape_nelts(c(2L, N))))
  expect_true(must_nelts_ne(c(2L, 3L), c(4L, 2L)))
  expect_false(must_nelts_ne(c(2L, 3L), c(3L, 2L)))
  # Unknown on either side defers.
  expect_false(must_nelts_ne(c(2L, N), c(3L, 2L)))
  expect_false(must_nelts_ne(c(2L, 3L), c(N, 2L)))
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

# ---- dynamic axis sizes ----------------------------------------------------

test_that("one dynamic executable runs at more than one size", {
  skip_if_no_iree_compile()

  # The whole point of the feature, as one assertion: compile *once* against
  # `tensor<?xf32>`, then run the same executable at three different sizes.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  src <- repr(hlo_return(hlo_add(a, b)))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  for (n in c(3L, 5L, 1L)) {
    x <- as.double(seq_len(n))
    v <- paste(x, collapse = " ")
    got <- iree_run(src, rep(sprintf("%dxf32=%s", n, v), 2L))
    expect_equal(got, x + x, tolerance = 1e-6, info = paste("n =", n))
  }
})

test_that("a refined type still runs, and inference did not lie", {
  skip_if_no_iree_compile()

  # `add(tensor<?xf32>, tensor<3xf32>)` infers `tensor<3xf32>`: inference
  # claims the dynamic side must be 3. Check the claim against a runtime that
  # can see the sizes, rather than trusting the inference that produced it.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", 3L)
  out <- hlo_add(a, b)
  expect_equal(repr(out$value_type$type), "tensor<3xf32>")

  got <- iree_run(
    repr(hlo_return(out)),
    c("3xf32=1 2 3", "3xf32=10 20 30")
  )
  expect_equal(got, c(11, 22, 33), tolerance = 1e-6)
})

test_that("a dynamic axis survives a chain of ops and reaches the right size", {
  skip_if_no_iree_compile()

  # concatenate is the interesting one: its on-axis size is the *sum*, so a
  # dynamic input gives a dynamic output, and only the runtime knows the
  # result is 2n long.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  src <- repr(hlo_return(hlo_concatenate(sel, a, dimension = 0L)))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  for (n in c(2L, 4L)) {
    x <- as.double(seq_len(n))
    y <- rep(1, n)
    got <- iree_run(
      src,
      c(
        sprintf("%dxf32=%s", n, paste(x, collapse = " ")),
        sprintf("%dxf32=%s", n, paste(y, collapse = " "))
      )
    )
    # select(x + y > x, x + y, x) is x + y wherever y > 0, i.e. everywhere.
    expect_equal(got, c(x + y, x), tolerance = 1e-6, info = paste("n =", n))
  }
})

test_that("the inferred dynamic types compile with IREE", {
  skip_if_no_iree_compile()

  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  cat2 <- hlo_concatenate(sel, a, dimension = 0L)
  src <- repr(hlo_return(cat2))

  expect_match(src, "tensor<?xf32>", fixed = TRUE)
  res <- iree_compiles(src)
  expect_true(res$ok, info = res$log)
})
