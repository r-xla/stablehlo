test_that("basic edge padding", {
  local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0L, dtype = "i32")
  y <- hlo_pad(
    x,
    pad_val,
    edge_padding_low = c(0, 1),
    edge_padding_high = c(2, 1),
    interior_padding = c(0, 0)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.integer(1:6), dim = c(2, 3))
  expected <- array(0L, dim = c(4, 5))
  expected[1:2, 2:4] <- input

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("interior padding", {
  local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0L, dtype = "i32")
  y <- hlo_pad(
    x,
    pad_val,
    edge_padding_low = c(0, 0),
    edge_padding_high = c(0, 0),
    interior_padding = c(1, 2)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.integer(1:6), dim = c(2, 3))

  expected <- array(0L, dim = c(3, 7))
  expected[1, 1] <- 1L
  expected[3, 1] <- 2L
  expected[1, 4] <- 3L
  expected[3, 4] <- 4L
  expected[1, 7] <- 5L
  expected[3, 7] <- 6L

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("errors", {
  check <- function(operand, padding_value, low, high, interior) {
    expect_snapshot(
      infer_types_pad(
        operand,
        padding_value,
        edge_padding_low = cnst(low, "i64", length(low)),
        edge_padding_high = cnst(high, "i64", length(high)),
        interior_padding = cnst(interior, "i64", length(interior))
      ),
      error = TRUE
    )
  }
  # (C3) interior_padding must be non-negative
  check(
    vt("f32", c(2L, 3L)),
    vt("f32", integer()),
    c(0L, 0L),
    c(0L, 0L),
    c(-1L, 0L)
  )
  # (C2) wrong length
  check(vt("f32", c(2L, 3L)), vt("f32", integer()), c(0L), c(0L, 0L), c(0L, 0L))
  # (I2) padding_value must be a 0-dimensional tensor
  check(
    vt("f32", c(2L, 3L)),
    vt("f32", 2L),
    c(0L, 0L),
    c(0L, 0L),
    c(0L, 0L)
  )
  # (C4) negative padding removing more than a dimension holds
  check(vt("f32", 3L), vt("f32", integer()), -3L, -3L, 0L)
  # (C4) the bound is the size of the dimension, not the rank of the operand,
  # so a rank-3 operand does not get to remove 3 elements from a size-1 one.
  check(
    vt("f32", c(1L, 5L, 5L)),
    vt("f32", integer()),
    c(-2L, 0L, 0L),
    c(0L, 0L, 0L),
    c(0L, 0L, 0L)
  )
})

test_that("negative edge padding trims, as long as the result is a shape", {
  pad <- function(shape, low, high) {
    shape(
      infer_types_pad(
        vt("f32", shape),
        vt("f32", integer()),
        edge_padding_low = cnst(low, "i64", length(low)),
        edge_padding_high = cnst(high, "i64", length(high)),
        interior_padding = cnst(rep(0L, length(shape)), "i64", length(shape))
      )[[1L]]
    )
  }
  # The amount trimmed is bounded by the size of the dimension it applies to,
  # which for a 1-dimensional operand is every value up to its own length.
  expect_equal(pad(10L, -5L, 0L), 5L)
  expect_equal(pad(10L, -1L, -1L), 8L)
  expect_equal(pad(10L, -10L, 0L), 0L)
  expect_equal(pad(c(5L, 5L), c(-4L, 0L), c(0L, 0L)), c(1L, 5L))
  # Mixed signs on one operand.
  expect_equal(pad(c(5L, 5L), c(-4L, 2L), c(0L, 3L)), c(1L, 10L))
})

test_that("negative edge padding executes as it infers", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "i32", shape = 10L)
  y <- hlo_pad(
    x,
    hlo_scalar(0L, dtype = "i32"),
    edge_padding_low = -5,
    edge_padding_high = 0,
    interior_padding = 0
  )
  f <- hlo_return(y)

  exec <- pjrt_compile(pjrt_program(repr(f)))
  output <- pjrt_execute(exec, pjrt_buffer(as.integer(1:10)))
  expect_equal(as_array(output), array(as.integer(6:10), dim = 5L))
})
