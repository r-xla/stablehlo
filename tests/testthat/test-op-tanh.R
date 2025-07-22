test_that("basic tests", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  z <- hlo_tanh(x)
  func <- hlo_return(z)
  expect_snapshot(repr(func))

  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  x <- array(c(-10, -1, 0, 1), dim = c(2, 2))
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(executable, x_buf)
  # tolerance in expect_equal needed to account for rounding errors
  expect_equal(tanh(x), as_array(out_buf), tolerance = 1e-6)
})
