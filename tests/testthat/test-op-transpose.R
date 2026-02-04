test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L, 4L))
  y <- hlo_transpose(x, permutation = c(2L, 0L, 1L))
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(1:24, dim = c(2, 3, 4))
  expected <- aperm(input, c(3, 1, 2)) # R's aperm uses 1-based indexing

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})

test_that("2D transpose", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "i32", shape = c(3L, 4L))
  y <- hlo_transpose(x, permutation = c(1L, 0L))
  f <- hlo_return(y)

  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(1:12, dim = c(3, 4))
  expected <- t(input)

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})

test_that("identity permutation", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L))
  y <- hlo_transpose(x, permutation = c(0L, 1L))
  f <- hlo_return(y)

  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(1:6, dim = c(2, 3))

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), input)
})

test_that("scalar transpose", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "f64", shape = integer())
  y <- hlo_transpose(x, permutation = integer())
  f <- hlo_return(y)

  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- 42.0

  output <- pjrt_execute(exec, pjrt_scalar(input, dtype = "f64"))
  expect_equal(as_array(output), input)
})

test_that("errors", {
  # (C2) invalid permutation
  expect_snapshot(
    infer_types_transpose(
      vt("f32", c(2L, 3L, 4L)),
      permutation = cnst(c(0L, 2L, 1L, 3L), "i64", 4L)
    ),
    error = TRUE
  )
})
