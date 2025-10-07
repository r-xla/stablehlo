test_that("basic tests", {
  x <- hlo_input("x", "f32", shape = c(4L, 3L))
  y <- hlo_slice(
    x,
    start_indices = c(2, 1),
    limit_indices = c(4, 3),
    strides = c(1, 1)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.numeric(1:12), dim = c(4, 3))

  expected <- input[3:4, 2:3]

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})
