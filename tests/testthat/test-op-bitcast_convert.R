test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  y <- hlo_bitcast_convert(
    x,
    cast_to_dtype = "ui8"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(seq(-1, 1, length.out = 6), dim = c(2, 3))

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )

  expect_equal(dim(as_array(output)), c(2, 3, 4))
  expect_true(all(as_array(output) >= 0))
})
