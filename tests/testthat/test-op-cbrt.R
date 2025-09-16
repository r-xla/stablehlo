test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  y <- hlo_cbrt(x)
  result_func <- hlo_return(y)
  expect_snapshot(repr(result_func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(result_func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  x <- array(c(1, 8, 27, 64), dim = c(2, 2))
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- as_array(out_buf)
  expect_equal(out, x^(1 / 3))
})
