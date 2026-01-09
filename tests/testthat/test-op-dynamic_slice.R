test_that("basic tests", {
  local_func()

  # Test from the spec
  x <- hlo_input("x", "i32", shape = c(4L, 4L))
  start0 <- hlo_scalar(-1L, dtype = "i64")
  start1 <- hlo_scalar(3L, dtype = "i64")

  y <- hlo_dynamic_slice(
    x,
    start0,
    start1,
    slice_sizes = c(2, 2)
  )

  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L),
    dim = c(4L, 4L)
  )

  expected <- array(c(1L, 1L, 1L, 1L), dim = c(2L, 2L))

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})
