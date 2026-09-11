test_that("basic tests", {
  local_func()

  # Test from the spec
  x <- hlo_input("x", "i32", shape = c(4L, 4L))
  start0 <- hlo_scalar(-1L, dtype = "i64")
  start1 <- hlo_scalar(3L, dtype = "i64")

  y <- hlo_dynamic_slice(
    x,
    start0,
    start1,
    slice_sizes = c(2, 2)
  )

  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L),
    dim = c(4L, 4L)
  )

  expected <- array(c(1L, 1L, 1L, 1L), dim = c(2L, 2L))

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("errors", {
  # (C2) wrong number of start_indices
  expect_snapshot(
    infer_types_dynamic_slice(
      vt("f32", c(4L, 5L)),
      vt("i32", integer()),
      slice_sizes = cnst(c(2L, 3L), "i64", 2L)
    ),
    error = TRUE
  )
  # (C2) wrong slice_sizes length
  expect_snapshot(
    infer_types_dynamic_slice(
      vt("f32", c(4L, 5L)),
      vt("i32", integer()),
      vt("i32", integer()),
      slice_sizes = cnst(c(2L), "i64", 1L)
    ),
    error = TRUE
  )
  # start_indices must be 0-dimensional
  expect_snapshot(
    infer_types_dynamic_slice(
      vt("f32", c(4L, 5L)),
      vt("i32", 2L),
      vt("i32", integer()),
      slice_sizes = cnst(c(2L, 3L), "i64", 2L)
    ),
    error = TRUE
  )
  # (C4) slice_sizes exceeds operand shape
  expect_snapshot(
    infer_types_dynamic_slice(
      vt("f32", c(4L, 5L)),
      vt("i32", integer()),
      vt("i32", integer()),
      slice_sizes = cnst(c(5L, 3L), "i64", 2L)
    ),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_slice and dynamic_update_slice defer their bounds", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_slice(
        dyn_input("a", "f32", N),
        hlo_scalar(0L, dtype = "i32"),
        slice_sizes = 2L
      )
    }),
    "tensor<2xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_dynamic_update_slice(
        dyn_input("a", "f32", N),
        dyn_input("u", "f32", 2L),
        hlo_scalar(0L, dtype = "i32")
      )
    }),
    "tensor<?xf32>"
  )
  # A slice that certainly overruns a known axis is still refused.
  local_func()
  expect_error(
    hlo_dynamic_slice(
      dyn_input("a", "f32", 3L),
      hlo_scalar(0L, dtype = "i32"),
      slice_sizes = 9L
    ),
    "must not be greater than"
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
