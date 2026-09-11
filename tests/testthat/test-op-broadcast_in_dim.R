test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(1L, 3L))
  y <- hlo_broadcast_in_dim(
    x,
    broadcast_dimensions = c(0L, 2L),
    shape = c(2L, 1L, 3L)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.double(c(1, 2, 3)), dim = c(1, 3))
  expected <- array(0, dim = c(2, 1, 3))
  expected[1, 1, ] <- input
  expected[2, 1, ] <- input

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})

test_that("append dims at the end", {
  local_func()
  x <- hlo_input("x", "f32", shape = 5L)
  y <- hlo_broadcast_in_dim(
    x,
    broadcast_dimensions = 0L,
    shape = c(5L, 2L)
  )
  f <- hlo_return(y)

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- pjrt_buffer(1:5, "f32")
  out <- pjrt_execute(exec, input)
  out_array <- as_array(out)

  expect_equal(shape(out), c(5L, 2L))
  expect_equal(out_array, array(c(1:5, 1:5), dim = c(5, 2)))
})

test_that("broadcasting a scalar", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "f32", 2L)
  f <- hlo_return(
    hlo_broadcast_in_dim(x, 0, c(2, 3))
  )
  exec <- pjrt_compile(pjrt_program(repr(f)))
  x <- pjrt_buffer(c(1, 1))
  expect_equal(
    pjrt_execute(exec, x),
    pjrt_buffer(1, shape = c(2, 3))
  )
})

test_that("works for scalars", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "f32", integer())
  f <- hlo_return(
    hlo_broadcast_in_dim(x, integer(), c(2, 3))
  )
  exec <- pjrt_compile(pjrt_program(repr(f)))
  x <- pjrt_scalar(1)
  expect_equal(
    pjrt_execute(exec, x),
    pjrt_buffer(1, shape = c(2, 3))
  )
})

test_that("errors", {
  check <- function(operand, broadcast_dimensions, shape) {
    expect_snapshot(
      infer_types_broadcast_in_dim(
        operand,
        broadcast_dimensions = cnst(
          broadcast_dimensions,
          "i64",
          length(broadcast_dimensions)
        ),
        shape = shape
      ),
      error = TRUE
    )
  }
  # (C2) broadcast_dimensions length != operand rank
  check(vt("f32", c(2L, 3L)), c(0L), c(4L, 5L, 6L))
  # (C3) broadcast_dimensions out of bounds
  check(vt("f32", c(2L, 3L)), c(0L, 5L), c(4L, 5L, 6L))
  # (C4) duplicate broadcast_dimensions
  check(vt("f32", c(2L, 3L)), c(0L, 0L), c(4L, 5L, 6L))
  # (C5) operand dim != 1 and != result dim
  check(vt("f32", c(2L, 3L)), c(0L, 1L), c(4L, 5L))
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("broadcast_in_dim leaves a dynamic axis to the runtime", {
  # A dynamic operand axis may be 1 (stretch) or 4 (pass through) at run time.
  expect_equal(
    inferred(function() {
      hlo_broadcast_in_dim(
        dyn_input("a", "f32", N),
        shape = c(2L, 4L),
        broadcast_dimensions = 1L
      )
    }),
    "tensor<2x4xf32>"
  )
  # Known, not 1, and not equal: certainly wrong.
  local_func()
  expect_error(
    hlo_broadcast_in_dim(
      dyn_input("a", "f32", 3L),
      shape = c(2L, 4L),
      broadcast_dimensions = 1L
    ),
    class = "ErrorDimSizeMismatch"
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

test_that("broadcast_in_dim rejects a dynamic shape attribute", {
  # The result is rendered as a static type, so `NA` must not reach it --
  # `hlo_dynamic_broadcast_in_dim()` is the escape hatch.
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  expect_error(
    hlo_broadcast_in_dim(x, broadcast_dimensions = c(0L, 1L), shape = c(N, 3L)),
    "Contains missing values"
  )
})
