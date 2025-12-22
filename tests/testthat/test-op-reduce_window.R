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
    body = red
  )
  func <- hlo_return(r)

  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  # 4x4 input reduced with 2x2 windows and stride 2 -> 2x2 output
  data <- matrix(c(
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12,
    13, 14, 15, 16
  ), nrow = 4L, byrow = TRUE)

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  # Expected: sum of each 2x2 window
  # [[1+2+5+6, 3+4+7+8], [9+10+13+14, 11+12+15+16]]
  expected <- array(c(14, 46, 22, 54), dim = c(2L, 2L))
  expect_equal(out, expected, tolerance = 1e-5)
})

test_that("reduce_window with max pooling", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(4L, 4L))
  # Use -Inf as init for max pooling
  init <- hlo_scalar(-Inf, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_maximum(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce_window(
    inputs = x,
    init_values = init,
    window_dimensions = c(2L, 2L),
    window_strides = c(2L, 2L),
    body = red
  )
  func <- hlo_return(r)

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(c(
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12,
    13, 14, 15, 16
  ), nrow = 4L, byrow = TRUE)

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  # Expected: max of each 2x2 window
  expected <- array(c(6, 14, 8, 16), dim = c(2L, 2L))
  expect_equal(out, expected, tolerance = 1e-5)
})

test_that("reduce_window with padding", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(3L, 3L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  # Add padding to make output same size as input
  r <- hlo_reduce_window(
    inputs = x,
    init_values = init,
    window_dimensions = c(3L, 3L),
    window_strides = c(1L, 1L),
    padding = matrix(c(1, 1, 1, 1), nrow = 2L, byrow = TRUE),
    body = red
  )
  func <- hlo_return(r)

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ), nrow = 3L, byrow = TRUE)

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  expect_equal(dim(out), c(3L, 3L))
})

test_that("reduce_window with window_dilations", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(5L, 5L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  # Window size 2 with dilation 2 means effective window size of 3
  r <- hlo_reduce_window(
    inputs = x,
    init_values = init,
    window_dimensions = c(2L, 2L),
    window_strides = c(1L, 1L),
    window_dilations = c(2L, 2L),
    body = red
  )
  func <- hlo_return(r)

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(1:25, nrow = 5L, byrow = TRUE)

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  # Output shape should be 3x3
  expect_equal(dim(out), c(3L, 3L))
})

test_that("reduce_window 1D", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(6L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce_window(
    inputs = x,
    init_values = init,
    window_dimensions = c(3L),
    window_strides = c(1L),
    body = red
  )
  func <- hlo_return(r)

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- array(c(1, 2, 3, 4, 5, 6), dim = 6L)

  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  # Window of 3 with stride 1: [1+2+3, 2+3+4, 3+4+5, 4+5+6]
  expected <- array(c(6, 9, 12, 15), dim = 4L)
  expect_equal(out, expected, tolerance = 1e-5)
})

