test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L, 2L))
  y <- hlo_reshape(
    x,
    shape_out = c(4L, 3L)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.double(1:12), dim = c(2, 3, 2))

  expected <- aperm(array(aperm(input, c(3, 2, 1)), dim = c(3, 4)), c(2, 1))

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})
