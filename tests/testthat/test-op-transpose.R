test_that("basic tests", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 3L, 2L), "main")
  p <- hlo_input("p", "si64", shape = c(3L), "main")
  z <- hlo_transpose(x, p)
  func <- hlo_return(z)
  expect_snapshot(repr(func))

  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  x <- array(1:12, dim = c(2, 3, 2))
  p <- array(c(2, 3, 1), dim = c(3))
  x_buf <- pjrt_buffer(x)
  p_buf <- pjrt_buffer(p)
  out_buf <- pjrt_execute(executable, x_buf, p_buf)
  expect_equal(aperm(x, p), as_array(out_buf))
})

# tens <- array(1:12, dim = c(2, 3, 2))
# aperm(tens, c(2, 3, 1))
