# ---- dynamic axis sizes ----------------------------------------------------

test_that("real_dynamic_slice", {
  expect_equal(
    inferred(function() {
      hlo_real_dynamic_slice(
        dyn_input("a", "f32", 8L),
        dyn_input("lo", "i32", 1L),
        dyn_input("hi", "i32", 1L),
        dyn_input("st", "i32", 1L),
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # The index vectors need one element per operand axis.
  local_func()
  expect_error(
    hlo_real_dynamic_slice(
      dyn_input("a", "f32", c(4L, 4L)),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 2L),
      dyn_input("st", "i32", 2L),
      shape = c(N, N)
    ),
    "one element per axis"
  )
  # The result keeps the operand's rank.
  local_func()
  expect_error(
    hlo_real_dynamic_slice(
      dyn_input("a", "f32", 8L),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 1L),
      dyn_input("st", "i32", 1L),
      shape = c(N, N)
    ),
    "same rank"
  )
})

test_that("a result extent can come from the data itself", {
  skip_if_no_iree_compile()
  # `a[0:n]`, where `n` is an *input value*, not a shape. Nothing in the
  # program determines the result's extent, so the type is `tensor<?xf32>` and
  # stays that way through refinement -- XLA refuses it, IREE runs it.
  local_func(id = "main")
  a <- dyn_input("a", "f32", 8L)
  n <- dyn_input("n", "i32", integer())
  out <- hlo_real_dynamic_slice(
    a,
    hlo_tensor(0L, dtype = "i32", shape = 1L),
    hlo_reshape(n, shape = 1L),
    hlo_tensor(1L, dtype = "i32", shape = 1L),
    shape = N
  )
  expect_equal(repr(out$value_type$type), "tensor<?xf32>")
  src <- repr(hlo_return(out))

  for (k in c(3L, 6L)) {
    got <- iree_run(src, c("8xf32=1 2 3 4 5 6 7 8", sprintf("i32=%d", k)))
    expect_equal(
      got,
      as.double(seq_len(k)),
      tolerance = 1e-6,
      info = paste("n =", k)
    )
  }
})

test_that("unique() is expressible", {
  skip_if_no_iree_compile()
  # sort, mark the first of each run, sort again by (keep desc, value asc) so
  # the survivors move to the front, then slice to a count that is itself a
  # reduction over the data. No scatter, and the output extent is data -- which
  # is what `real_dynamic_slice` is for.
  n <- 8L
  local_func(id = "main")
  a <- dyn_input("a", "f32", n)

  sorted <- hlo_sort(
    a,
    dimension = 0L,
    is_stable = TRUE,
    comparator = lt_region()
  )[[1L]]
  head_ <- hlo_slice(
    sorted,
    start_indices = 0L,
    limit_indices = n - 1L,
    strides = 1L
  )
  tail_ <- hlo_slice(
    sorted,
    start_indices = 1L,
    limit_indices = n,
    strides = 1L
  )
  ne <- hlo_compare(
    tail_,
    head_,
    comparison_direction = "NE",
    compare_type = "FLOAT"
  )
  first <- hlo_tensor(TRUE, dtype = "bool", shape = 1L)
  keep <- hlo_concatenate(first, ne, dimension = 0L)
  keep_i <- hlo_convert(keep, dtype = "i32")

  resorted <- hlo_sort(
    keep_i,
    sorted,
    dimension = 0L,
    is_stable = TRUE,
    comparator = keep_desc_value_asc_region()
  )
  count <- hlo_reduce(
    list(keep_i),
    list(hlo_scalar(0L, dtype = "i32")),
    body = add_region("i32"),
    dimensions = 0L
  )
  out <- hlo_real_dynamic_slice(
    resorted[[2L]],
    hlo_tensor(0L, dtype = "i32", shape = 1L),
    hlo_reshape(count, shape = 1L),
    hlo_tensor(1L, dtype = "i32", shape = 1L),
    shape = N
  )
  expect_equal(repr(out$value_type$type), "tensor<?xf32>")
  src <- repr(hlo_return(out))

  for (v in list(c(3, 1, 3, 2, 1, 3, 2, 1), rep(5, 8), as.double(1:8))) {
    got <- iree_run(src, sprintf("8xf32=%s", paste(v, collapse = " ")))
    expect_equal(
      got,
      sort(unique(v)),
      tolerance = 1e-6,
      info = paste(v, collapse = " ")
    )
  }
})

test_that("the index operands must be integers of one identical type", {
  # SPEC types all three "1-dimensional tensor of integer type", and the ODS
  # requires one identical type across them -- neither of which the static
  # twin needed to state, because there the sizes were an i64 constant.
  rds <- function(s, l, t) {
    local_func()
    hlo_real_dynamic_slice(
      dyn_input("a", "f32", 8L),
      dyn_input("s", s, 1L),
      dyn_input("l", l, 1L),
      dyn_input("t", t, 1L),
      shape = N
    )
  }
  expect_error(rds("f32", "i32", "i32"), "must have dtype int or uint")
  expect_error(rds("i32", "i64", "i32"), "same type")
  expect_no_error(rds("i32", "i32", "i32"))
  # Unlike the rest of the family these are `HLO_DimensionTensor`, so their
  # own extent is allowed to be dynamic.
  local_func()
  expect_equal(
    repr(
      hlo_real_dynamic_slice(
        dyn_input("a", "f32", 8L),
        dyn_input("s", "i32", N),
        dyn_input("l", "i32", N),
        dyn_input("t", "i32", N),
        shape = N
      )$value_type$type
    ),
    "tensor<?xf32>"
  )
})

test_that("the result takes the shape hint, not a blanket dynamic shape", {
  expect_equal(
    inferred(function() {
      hlo_real_dynamic_slice(
        dyn_input("a", "f32", c(N, N)),
        dyn_input("s", "i32", 2L),
        dyn_input("l", "i32", 2L),
        dyn_input("t", "i32", 2L),
        shape = c(2L, 3L)
      )
    }),
    "tensor<2x3xf32>"
  )
})
