test_that("basic tests", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  y <- hlo_input("y", "f32", shape = c(2L, 2L), "main")
  z <- hlo_divide(x, y)
  func <- hlo_return(z)
  expect_snapshot(repr(func))

  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  x <- array(c(1, 2, 3, 4), dim = c(2, 2))
  y <- array(c(5, 6, 7, 8), dim = c(2, 2))
  x_buf <- pjrt_buffer(x)
  y_buf <- pjrt_buffer(y)
  out_buf <- pjrt_execute(executable, x_buf, y_buf)
  expect_equal(x / y, as_array(out_buf), tolerance = 1e-6)
})
