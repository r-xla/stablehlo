#test_that("basic tests", {
#  func <- local_func()
#  x <- hlo_input("x", "f64", shape = c(3L, 3L))
#  y <- hlo_cholesky(
#    x,
#    lower = TRUE
#  )
#  f <- hlo_return(y)
#  expect_snapshot(repr(f))
#
#  skip_if_not_installed("pjrt")
#  program <- pjrt_program(repr(f))
#  exec <- pjrt_compile(program)
#
#  M <- withr::with_seed(1, matrix(runif(9), 3, 3))
#  input <- t(M) %*% M
#  expected <- t(chol(input))
#
#  output <- pjrt_execute(exec, pjrt_buffer(input, dtype = "f64"))
#  expect_equal(as_array(output), expected, tolerance = 1e-3)
#})
#
