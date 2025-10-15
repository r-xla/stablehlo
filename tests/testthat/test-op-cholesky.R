test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(3L, 3L))
  y <- hlo_cholesky(
    x,
    lower = FALSE
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  M <- withr::with_seed(1, M <- matrix(runif(9), 3, 3))
  input <- array(t(M) %*% M, dim = c(3, 3))
  expected <- array(chol(input), dim = c(3, 3))

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected, tolerance = 1e-3)
})
