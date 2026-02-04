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
})

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
