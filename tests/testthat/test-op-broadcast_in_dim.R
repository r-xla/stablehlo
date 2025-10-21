test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(1L, 3L))
  y <- hlo_broadcast_in_dim(
    x,
    broadcast_dimensions = c(0L, 2L),
    shape_out = c(2L, 1L, 3L)
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
    shape_out = c(5L, 2L)
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
