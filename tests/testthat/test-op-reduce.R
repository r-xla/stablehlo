test_that("errors", {
  body <- local_func("body")
  x <- hlo_input("x", "f32")
  y <- hlo_input("y", "f32")
  body <- hlo_return(hlo_add(x, y))
  # (C3) no inputs
  expect_snapshot(
    infer_types_reduce(
      inputs = list(),
      init_values = list(),
      body = body,
      dimensions = cnst(0L, "i64", 1L)
    ),
    error = TRUE
  )
  # (C3) inputs/init_values count mismatch
  expect_snapshot(
    infer_types_reduce(
      inputs = list(vt("f32", c(2L, 3L))),
      init_values = list(vt("f32", integer()), vt("f32", integer())),
      body = body,
      dimensions = cnst(0L, "i64", 1L)
    ),
    error = TRUE
  )
  # init_values must be 0-D
  expect_snapshot(
    infer_types_reduce(
      inputs = list(vt("f32", c(2L, 3L))),
      init_values = list(vt("f32", 2L)),
      body = body,
      dimensions = cnst(0L, "i64", 1L)
    ),
    error = TRUE
  )
  # (C4) dimension out of bounds
  expect_snapshot(
    infer_types_reduce(
      inputs = list(vt("f32", c(2L, 3L))),
      init_values = list(vt("f32", integer())),
      body = body,
      dimensions = cnst(5L, "i64", 1L)
    ),
    error = TRUE
  )
  # (C5) duplicate dimensions
  expect_snapshot(
    infer_types_reduce(
      inputs = list(vt("f32", c(2L, 3L))),
      init_values = list(vt("f32", integer())),
      body = body,
      dimensions = cnst(c(0L, 0L), "i64", 2L)
    ),
    error = TRUE
  )
})

test_that("basic tests", {
  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce(inputs = x, init_values = init, dimensions = 1L, body = red)
  func <- hlo_return(r)

  expect_snapshot(func)
  skip_if_not_installed("pjrt")

  expect_snapshot(repr(func))

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2L, byrow = TRUE)
  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  expect_equal(as.numeric(out), rowSums(data), tolerance = 1e-5)
})

test_that("reduce along multiple dimensions", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  init <- hlo_scalar(0, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce(
    inputs = x,
    init_values = init,
    dimensions = c(0L, 1L),
    body = red
  )
  func <- hlo_return(r)

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2L, byrow = TRUE)
  x_buf <- pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf)
  out <- as_array(out_buf)

  expect_equal(as.numeric(out), sum(data), tolerance = 1e-5)
})

test_that("reduce with two different tensors and init values", {
  skip_if_not_installed("pjrt")

  func <- local_func()

  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  y <- hlo_input("y", "f32", shape = c(2L, 3L))

  init_x <- hlo_scalar(0, dtype = "f32")
  init_y <- hlo_scalar(1, dtype = "f32")

  red <- local_func("reducer")
  a <- hlo_input("a", "f32")
  b <- hlo_input("b", "f32")
  c <- hlo_input("c", "f32")
  d <- hlo_input("d", "f32")

  sum_x <- hlo_add(a, c)
  sum_y <- hlo_add(b, d)

  red <- hlo_return(sum_x, sum_y)

  r <- hlo_reduce(
    inputs = list(x, y),
    init_values = list(init_x, init_y),
    dimensions = 1L,
    body = red
  )
  func <- hlo_return(r[[1]], r[[2]])

  program <- pjrt_program(repr(func))
  executable <- pjrt_compile(program)

  data_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2L, byrow = TRUE)
  data_y <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 2L, byrow = TRUE)

  x_buf <- pjrt_buffer(data_x, dtype = "f32")
  y_buf <- pjrt_buffer(data_y, dtype = "f32")
  out_buf <- pjrt_execute(executable, x_buf, y_buf)
  out1 <- as_array(out_buf[[1]])
  out2 <- as_array(out_buf[[2]])

  expect_equal(out1, array(c(6, 15), dim = 2L))
  expect_equal(out2, array(c(10, 19), dim = 2L))
})
