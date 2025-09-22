test_that("reduce with addition prints correctly", {
  func <- local_func()

  x <- hlo_input("x", "i64", shape = c(1L, 6L))
  init <- hlo_scalar(0L, dtype = "i64")

  # Build reducer body: (a, b) -> add(a, b)
  red <- local_func("reducer")
  a <- hlo_input("a", "i64")
  b <- hlo_input("b", "i64")
  s <- hlo_add(a, b)
  red <- hlo_return(s)

  r <- hlo_reduce(
    inputs = x,
    init_values = init,
    dimensions = 1L,
    body = red
  )

  func <- hlo_return(r)

  expect_snapshot(repr(func))
})


test_that("reduce with addition executes", {
  skip_if_not_installed("pjrt")

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

  expect_snapshot(repr(func))

  program <- pjrt::pjrt_program(repr(func))
  executable <- pjrt::pjrt_compile(program)

  data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2L, byrow = TRUE)
  x_buf <- pjrt::pjrt_buffer(data, dtype = "f32")
  out_buf <- pjrt::pjrt_execute(executable, x_buf)
  out <- pjrt::as_array(out_buf)

  expect_equal(as.numeric(out), rowSums(data), tolerance = 1e-5)
})
