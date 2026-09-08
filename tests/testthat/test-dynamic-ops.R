# The dynamic-op family: the ops that take their sizes as *operands* rather
# than attributes. Their result extents are therefore data, which is why each
# takes a `shape` hint -- inference cannot derive what only the runtime knows.

test_that("dynamic_iota", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_iota(
        dyn_input("s", "i32", 1L),
        iota_dimension = 0L,
        dtype = "f32",
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # A static hint is fine too -- the sizes are still supplied at run time.
  expect_equal(
    inferred(function() {
      hlo_dynamic_iota(
        dyn_input("s", "i32", 2L),
        iota_dimension = 1L,
        dtype = "i32",
        shape = c(2L, N)
      )
    }),
    "tensor<2x?xi32>"
  )
  local_func()
  expect_error(
    hlo_dynamic_iota(
      dyn_input("s", "i32", 1L),
      iota_dimension = 3L,
      dtype = "f32",
      shape = N
    ),
    "iota_dimension"
  )
  # output_shape must have one element per result axis.
  local_func()
  expect_error(
    hlo_dynamic_iota(
      dyn_input("s", "i32", 3L),
      iota_dimension = 0L,
      dtype = "f32",
      shape = c(N, N)
    ),
    "one element per axis"
  )
})

test_that("dynamic_reshape", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_reshape(
        dyn_input("a", "f32", N),
        dyn_input("s", "i32", 2L),
        shape = c(N, 3L)
      )
    }),
    "tensor<?x3xf32>"
  )
  # Element counts known on both sides and unequal: still refused.
  local_func()
  expect_error(
    hlo_dynamic_reshape(
      dyn_input("a", "f32", 5L),
      dyn_input("s", "i32", 2L),
      shape = c(2L, 3L)
    ),
    "same number of elements"
  )
  # A dynamic axis on either side defers that check -- the point of the op.
  expect_equal(
    inferred(function() {
      hlo_dynamic_reshape(
        dyn_input("a", "f32", N),
        dyn_input("s", "i32", 2L),
        shape = c(2L, 3L)
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("dynamic_pad", {
  pad3 <- function() dyn_input(paste0("p", sample.int(1e6, 1L)), "i32", 1L)
  expect_equal(
    inferred(function() {
      hlo_dynamic_pad(
        dyn_input("a", "f32", N),
        hlo_scalar(0, dtype = "f32"),
        dyn_input("lo", "i32", 1L),
        dyn_input("hi", "i32", 1L),
        dyn_input("in", "i32", 1L),
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # The padding vectors need one element per operand axis.
  local_func()
  expect_error(
    hlo_dynamic_pad(
      dyn_input("a", "f32", c(N, 3L)),
      hlo_scalar(0, dtype = "f32"),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 2L),
      dyn_input("in", "i32", 2L),
      shape = c(N, N)
    ),
    "one element per axis"
  )
  # padding_value must be a scalar of the operand's type.
  local_func()
  expect_error(
    hlo_dynamic_pad(
      dyn_input("a", "f32", N),
      dyn_input("pv", "f64", integer()),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 1L),
      dyn_input("in", "i32", 1L),
      shape = N
    ),
    "same data type"
  )
})

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

# ---- the point of real_dynamic_slice: an extent computed from data ---------

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

test_that("dynamic_gather", {
  # The batch axes come from `start_indices` and stay static; the offset axes
  # come from `slice_sizes`, which is now data, so they are `?`.
  expect_equal(
    inferred(function() {
      hlo_dynamic_gather(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("i", "i32", c(2L, 1L)),
        dyn_input("sz", "i32", 2L),
        gather_dimension_numbers = GatherDimensionNumbers(
          offset_dims = 1L,
          collapsed_slice_dims = 0L,
          start_index_map = 0L,
          index_vector_dim = 1L
        )
      )
    }),
    "tensor<2x?xf32>"
  )
  # It still enforces everything `gather` does -- the inference is shared, with
  # the extents marked unknown.
  local_func()
  expect_error(
    hlo_dynamic_gather(
      dyn_input("a", "f32", c(N, 3L)),
      dyn_input("i", "i32", c(2L, 1L)),
      dyn_input("sz", "i32", 5L),
      gather_dimension_numbers = GatherDimensionNumbers(
        offset_dims = 1L,
        collapsed_slice_dims = 0L,
        start_index_map = 0L,
        index_vector_dim = 1L
      )
    ),
    "one element per axis"
  )
})

test_that("dynamic_conv", {
  # Batch and feature axes stay static; the spatial extents depend on the
  # padding, which is now data.
  expect_equal(
    inferred(function() {
      hlo_dynamic_conv(
        dyn_input("a", "f32", c(2L, 1L, 4L)),
        dyn_input("k", "f32", c(1L, 1L, 2L)),
        dyn_input("p", "i32", c(1L, 2L)),
        dimension_numbers = ConvDimensionNumbers(
          0L,
          1L,
          2L,
          1L,
          0L,
          2L,
          0L,
          1L,
          2L
        ),
        window_strides = 1L
      )
    }),
    "tensor<2x1x?xf32>"
  )
  # A dynamic batch axis stays dynamic too.
  expect_equal(
    inferred(function() {
      hlo_dynamic_conv(
        dyn_input("a", "f32", c(N, 1L, 4L)),
        dyn_input("k", "f32", c(1L, 1L, 2L)),
        dyn_input("p", "i32", c(1L, 2L)),
        dimension_numbers = ConvDimensionNumbers(
          0L,
          1L,
          2L,
          1L,
          0L,
          2L,
          0L,
          1L,
          2L
        ),
        window_strides = 1L
      )
    }),
    "tensor<?x1x?xf32>"
  )
  # `padding` must have one row per spatial axis.
  local_func()
  expect_error(
    hlo_dynamic_conv(
      dyn_input("a", "f32", c(2L, 1L, 4L)),
      dyn_input("k", "f32", c(1L, 1L, 2L)),
      dyn_input("p", "i32", c(3L, 2L)),
      dimension_numbers = ConvDimensionNumbers(
        0L,
        1L,
        2L,
        1L,
        0L,
        2L,
        0L,
        1L,
        2L
      ),
      window_strides = 1L
    ),
    "one row per spatial axis"
  )
})
