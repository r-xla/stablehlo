test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "i32", shape = 2L)
  y <- hlo_count_leading_zeros(x)
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- c(0L, 1L)
  expected <- pjrt_buffer(c(32L, 31L))

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(output, expected)
})
