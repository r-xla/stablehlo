# A dynamic program is only worth emitting if it is a real program. Each test
# here builds one with dynamic axes, refines it to concrete argument types with
# `pjrt_refine_shapes()`, compiles it with XLA and runs it -- and checks that
# the type the refiner derives is the one our own inference derives from the
# static shapes. See expect_refines_and_runs() in helper-dynamic.R.

test_that("elementwise chain", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_multiply(hlo_add(a, b), a)
    },
    dyn_shapes = list(N, N),
    runs = list(
      list(shapes = list(3L, 3L), args = list(c(1, 2, 3), c(10, 20, 30))),
      list(shapes = list(5L, 5L), args = list(1:5 + 0, rep(2, 5)))
    )
  )
})

test_that("a dynamic operand mixed with a static one", {
  skip_if_no_refine()
  # The second argument's shape is static in the program, so refinement only
  # has the first to resolve -- and our inference already refined the result to
  # the static shape.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_add(a, b)
    },
    dyn_shapes = list(N, 4L),
    runs = list(list(shapes = list(4L, 4L), args = list(1:4 + 0, rep(10, 4))))
  )
})

test_that("compare and select", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      gt <- hlo_compare(
        a,
        b,
        comparison_direction = "GT",
        compare_type = "FLOAT"
      )
      hlo_select(gt, a, b)
    },
    dyn_shapes = list(N, N),
    runs = list(
      list(shapes = list(4L, 4L), args = list(c(1, 5, 3, 7), c(2, 2, 9, 4))),
      list(shapes = list(2L, 2L), args = list(c(8, 1), c(3, 6)))
    )
  )
})

test_that("concatenate: the refiner derives the sum we could not", {
  skip_if_no_refine()
  # Our inference says `?` for the concatenated axis; the refiner proves 2n.
  # This is the case where the two disagree in strength but must not disagree
  # in fact.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      hlo_concatenate(a, a, dimension = 0L)
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(3L), args = list(c(1, 2, 3))),
      list(shapes = list(6L), args = list(1:6 + 0))
    )
  )
})

test_that("reduce over a dynamic axis", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      x <- dyn_input("x", "f32", shapes[[1L]])
      hlo_reduce(
        list(x),
        list(hlo_scalar(0, dtype = "f32")),
        body = add_region(),
        dimensions = 0L
      )
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(4L), args = list(c(1, 2, 3, 4))),
      list(shapes = list(7L), args = list(1:7 + 0))
    )
  )
})

test_that("reduce over a static axis of a dynamic operand", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      x <- dyn_input("x", "f32", shapes[[1L]])
      hlo_reduce(
        list(x),
        list(hlo_scalar(0, dtype = "f32")),
        body = add_region(),
        dimensions = 0L
      )
    },
    dyn_shapes = list(c(N, 3L)),
    runs = list(
      list(shapes = list(c(2L, 3L)), args = list(1:6 + 0)),
      list(shapes = list(c(5L, 3L)), args = list(1:15 + 0))
    )
  )
})

test_that("transpose", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      hlo_transpose(
        dyn_input("a", "f32", shapes[[1L]]),
        permutation = c(1L, 0L)
      )
    },
    dyn_shapes = list(c(N, 3L)),
    runs = list(
      list(shapes = list(c(2L, 3L)), args = list(1:6 + 0)),
      list(shapes = list(c(4L, 3L)), args = list(1:12 + 0))
    )
  )
})

test_that("dot_general with a dynamic batch axis", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_dot_general(a, b, contracting_dims = list(1L, 0L))
    },
    dyn_shapes = list(c(N, 3L), c(3L, 2L)),
    runs = list(
      list(shapes = list(c(2L, 3L), c(3L, 2L)), args = list(1:6 + 0, 1:6 + 0)),
      list(shapes = list(c(4L, 3L), c(3L, 2L)), args = list(1:12 + 0, 1:6 + 0))
    )
  )
})

test_that("broadcast_in_dim from an axis whose size is deferred", {
  skip_if_no_refine()
  # Our inference cannot check the operand's axis against the target, so it
  # defers; refinement resolves it and XLA compiles the result.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      hlo_broadcast_in_dim(a, broadcast_dimensions = 1L, shape = c(2L, 3L))
    },
    dyn_shapes = list(N),
    runs = list(list(shapes = list(3L), args = list(c(1, 2, 3))))
  )
})

test_that("scalar broadcast via get_dimension_size", {
  skip_if_no_refine()
  # The pair of ops this branch added: read the size off the value, then
  # broadcast to it. Refinement has to fold the read into a constant.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      size <- hlo_reshape(hlo_get_dimension_size(a, dimension = 0L), shape = 1L)
      two <- hlo_dynamic_broadcast_in_dim(
        hlo_scalar(2, dtype = "f32"),
        size,
        broadcast_dimensions = integer(),
        shape = shapes[[1L]]
      )
      hlo_multiply(a, two)
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(3L), args = list(c(1, 2, 3))),
      list(shapes = list(5L), args = list(1:5 + 0))
    )
  )
})

test_that("gather over a dynamic operand", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      hlo_gather(
        dyn_input("a", "f32", shapes[[1L]]),
        dyn_input("i", "i32", shapes[[2L]]),
        gather_dimension_numbers = GatherDimensionNumbers(
          offset_dims = 1L,
          collapsed_slice_dims = 0L,
          start_index_map = 0L,
          index_vector_dim = 1L
        ),
        slice_sizes = c(1L, 3L)
      )
    },
    dyn_shapes = list(c(N, 3L), c(2L, 1L)),
    dtype = c("f32", "i32"),
    runs = list(
      list(
        shapes = list(c(4L, 3L), c(2L, 1L)),
        args = list(1:12 + 0, c(0L, 2L))
      ),
      list(
        shapes = list(c(6L, 3L), c(2L, 1L)),
        args = list(1:18 + 0, c(1L, 4L))
      )
    )
  )
})

test_that("dynamic_slice of a dynamic operand", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      s <- dyn_input("s", "i32", shapes[[2L]])
      hlo_dynamic_slice(a, s, slice_sizes = 2L)
    },
    dyn_shapes = list(N, integer()),
    dtype = c("f32", "i32"),
    runs = list(
      list(shapes = list(5L, integer()), args = list(1:5 + 0, 1L)),
      list(shapes = list(8L, integer()), args = list(1:8 + 0, 3L))
    )
  )
})

test_that("convolution over a dynamic batch axis", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      hlo_convolution(
        dyn_input("a", "f32", shapes[[1L]]),
        dyn_input("k", "f32", shapes[[2L]]),
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
        window_strides = 1L,
        padding = matrix(0L, 1L, 2L),
        lhs_dilation = 1L,
        rhs_dilation = 1L
      )
    },
    dyn_shapes = list(c(N, 1L, 4L), c(1L, 1L, 2L)),
    runs = list(
      list(
        shapes = list(c(2L, 1L, 4L), c(1L, 1L, 2L)),
        args = list(1:8 + 0, c(1, 1))
      ),
      list(
        shapes = list(c(3L, 1L, 4L), c(1L, 1L, 2L)),
        args = list(1:12 + 0, c(1, -1))
      )
    )
  )
})

test_that("sort over a dynamic axis", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      hlo_sort(
        dyn_input("x", "f32", shapes[[1L]]),
        dimension = 0L,
        is_stable = TRUE,
        comparator = lt_region()
      )[[1L]]
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(4L), args = list(c(3, 1, 4, 2))),
      list(shapes = list(6L), args = list(c(9, 2, 7, 1, 8, 3)))
    )
  )
})
