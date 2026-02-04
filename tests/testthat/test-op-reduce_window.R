test_that("basic reduce_window (sum pooling)", {
  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(4L, 4L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce_window(
    inputs = x,
    init_values = init,
    window_dimensions = c(2L, 2L),
    window_strides = c(2L, 2L),
    base_dilations = c(1L, 1L),
    window_dilations = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L),
    body = red
  )
  func <- hlo_return(r)

  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(
    c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16
    ),
    nrow = 4L,
    byrow = TRUE
  )

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  # Expected: sum of each 2x2 window
  expected <- array(c(14, 46, 22, 54), dim = c(2L, 2L))
  expect_equal(out, expected, tolerance = 1e-5)
})

test_that("errors", {
  body <- local_func("body")
  x <- hlo_input("x", "f32")
  y <- hlo_input("y", "f32")
  body <- hlo_return(hlo_add(x, y))
  pad2 <- cnst(c(0L, 0L, 0L, 0L), "i64", c(2L, 2L))
  s2 <- cnst(c(1L, 1L), "i64", 2L)
  d2 <- cnst(c(1L, 1L), "i64", 2L)
  # odd number of arguments
  expect_snapshot(
    infer_types_reduce_window(
      vt("f32", c(4L, 4L)),
      body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L),
      window_strides = s2,
      base_dilations = d2,
      window_dilations = d2,
      padding = pad2
    ),
    error = TRUE
  )
  # window_dimensions wrong length
  expect_snapshot(
    infer_types_reduce_window(
      vt("f32", c(4L, 4L)),
      vt("f32", integer()),
      body = body,
      window_dimensions = cnst(c(2L, 2L, 2L), "i64", 3L),
      window_strides = s2,
      base_dilations = d2,
      window_dilations = d2,
      padding = pad2
    ),
    error = TRUE
  )
  # window_dimensions must be positive
  expect_snapshot(
    infer_types_reduce_window(
      vt("f32", c(4L, 4L)),
      vt("f32", integer()),
      body = body,
      window_dimensions = cnst(c(0L, 2L), "i64", 2L),
      window_strides = s2,
      base_dilations = d2,
      window_dilations = d2,
      padding = pad2
    ),
    error = TRUE
  )
  # window_strides must be positive
  expect_snapshot(
    infer_types_reduce_window(
      vt("f32", c(4L, 4L)),
      vt("f32", integer()),
      body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L),
      window_strides = cnst(c(0L, 1L), "i64", 2L),
      base_dilations = d2,
      window_dilations = d2,
      padding = pad2
    ),
    error = TRUE
  )
})
