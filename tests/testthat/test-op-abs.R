test_that("basic tests", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  y <- hlo_abs(x)
  func <- hlo_return(y)
  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  x <- array(c(-1, 2, -3, 4), dim = c(2, 2))
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- as_array(out_buf)
  expect_equal(out, abs(x))
})
