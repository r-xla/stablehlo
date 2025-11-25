test_that("basic tests", {
  func <- local_func()
  x1 <- hlo_input("x1", "i32", shape = c(3L, 1L))
  x2 <- hlo_input("x2", "i32", shape = c(3L, 2L))
  x3 <- hlo_input("x3", "i32", shape = c(3L, 3L))
  y <- hlo_concatenate(
    x1,
    x2,
    x3,
    dimension = 2L
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  x1 <- array(1L:3L, dim = c(3, 1))
  x2 <- array(1L:6L, dim = c(3, 2))
  x3 <- array(1L:9L, dim = c(3, 3))
  expected <- cbind(x1, x2, x3)

  output <- pjrt_execute(
    exec,
    pjrt_buffer(x1),
    pjrt_buffer(x2),
    pjrt_buffer(x3)
  )
  expect_equal(as_array(output), expected, tolerance = 1e-3)
})
