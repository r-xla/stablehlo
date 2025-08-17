test_that("basic tests", {
  local_reset_id_gen()
  a <- hlo_input("a", "f32", shape = c(2L, 2L, 2L), "main")
  # lower <- hlo_input("lower", "i1", shape = 1L, "main") # ToDo: holo_constant / hlo_scalar
  y <- hlo_cholesky(a, lower)
  func <- hlo_return(y)
  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  a <- array(cbind(matrix(c(2, 1, 1, 2), 2, 2), diag(1, 2)), dim = c(2, 2, 2))
  lower <- TRUE
  a_buf <- pjrt_buffer(a)
  lower_buf <- pjrt_buffer(lower)
  out_buf <- pjrt_execute(executable, a_buf, lower_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- as_array(out_buf)
  out_expected <- array(apply(a, 3, chol, simplify = TRUE), dim = c(2,2,2))
  if (lower) {
    out_expected <- array(apply(out_expected, 3, t, simplify = TRUE), dim = c(2,2,2))
  }
  expect_equal(out, out_expected)
})
