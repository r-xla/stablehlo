test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(4L, 3L))
  y <- hlo_slice(
    x,
    start_indices = c(2, 1),
    limit_indices = c(4, 3),
    strides = c(1, 1)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.numeric(1:12), dim = c(4, 3))

  expected <- input[3:4, 2:3]

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("errors", {
  check <- function(operand, start, limit, strides) {
    expect_snapshot(
      infer_types_slice(
        operand,
        start_indices = cnst(start, "i64", length(start)),
        limit_indices = cnst(limit, "i64", length(limit)),
        strides = cnst(strides, "i64", length(strides))
      ),
      error = TRUE
    )
  }
  # (C2) length mismatch start vs limit
  check(vt("f32", c(4L, 5L)), 0L, c(2L, 3L), c(1L, 1L))
  # (C2) length != operand rank
  check(vt("f32", c(4L, 5L)), c(0L, 0L, 0L), c(2L, 3L, 4L), c(1L, 1L, 1L))
  # (C3) start_indices < 0
  check(vt("f32", c(4L, 5L)), c(-1L, 0L), c(2L, 3L), c(1L, 1L))
  # (C3) start > limit
  check(vt("f32", c(4L, 5L)), c(3L, 0L), c(2L, 3L), c(1L, 1L))
  # (C3) limit > operand_shape
  check(vt("f32", c(4L, 5L)), c(0L, 0L), c(4L, 6L), c(1L, 1L))
  # (C4) strides must be non-negative
  check(vt("f32", c(4L, 5L)), c(0L, 0L), c(2L, 3L), c(-1L, 1L))
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("slice and dynamic_slice defer their bounds against a dynamic axis", {
  # limit 4 on an axis of unknown size: legal if the operand is long enough.
  expect_equal(
    inferred(function() {
      hlo_slice(
        dyn_input("a", "f32", N),
        start_indices = 0L,
        limit_indices = 4L,
        strides = 1L
      )
    }),
    "tensor<4xf32>"
  )
  # Known and too short: refused.
  local_func()
  expect_error(
    hlo_slice(
      dyn_input("a", "f32", 3L),
      start_indices = 0L,
      limit_indices = 4L,
      strides = 1L
    ),
    class = "ErrorIndexOutOfBounds"
  )
})
