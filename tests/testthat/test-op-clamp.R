test_that("basic tests", {
  func <- local_func()
  lo <- hlo_input("lo", "f32", shape = c(2L, 2L))
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  hi <- hlo_input("hi", "f32", shape = c(2L, 2L))
  y <- hlo_clamp(lo, x, hi)
  result_func <- hlo_return(y)
  expect_snapshot(repr(result_func))

  program <- pjrt_program(repr(result_func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  lo <- array(c(1, 2, 3, 4), dim = c(2, 2))
  hi <- array(c(5, 6, 7, 8), dim = c(2, 2))
  x <- array(c(0, 5, 8, 2), dim = c(2, 2))
  lo_buf <- pjrt_buffer(lo)
  hi_buf <- pjrt_buffer(hi)
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(executable, lo_buf, x_buf, hi_buf)
  expect_equal(pmin(pmax(lo, x), hi), as_array(out_buf))
})
