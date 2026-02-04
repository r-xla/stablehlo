test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f64", shape = c(3L, 3L))
  y <- hlo_cholesky(
    x,
    lower = FALSE
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  M <- withr::with_seed(1, matrix(runif(9), 3, 3))
  input <- t(M) %*% M
  expected <- chol(input)

  output <- pjrt_execute(exec, pjrt_buffer(input, dtype = "f64"))
  # The content of the other half of the matrix is not guaranteed and backend dependent
  output <- as_array(output)
  output[lower.tri(output)] <- 0
  expect_equal(output, expected, tolerance = 1e-3)
})

test_that("errors", {
  check <- function(operand) {
    expect_snapshot(
      infer_types_cholesky(operand, lower = scnst(TRUE, "pred")),
      error = TRUE
    )
  }
  # (C2) rank < 2
  check(vt("f32", 3L))
  # (C3) last two dimensions not equal
  check(vt("f32", c(3L, 4L)))
})
