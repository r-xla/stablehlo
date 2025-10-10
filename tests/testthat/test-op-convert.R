test_that("integer to float conversion", {
  skip_if_not_installed("pjrt")

  local_func()
  operand <- hlo_input("operand", "i32", shape = 4)
  result <- hlo_convert(operand, "f32")
  f <- hlo_return(result)
  expect_snapshot(repr(f))

  program <- pjrt_program(repr(f))
  executable <- pjrt_compile(program)

  x <- array(c(1L, 2L, 3L, 4L), dim = 4)
  x_buf <- pjrt_buffer(x, dtype = "i32")
  out_buf <- pjrt_execute(executable, x_buf)

  expect_equal(
    out_buf,
    pjrt_buffer(array(c(1, 2, 3, 4), dim = 4), dtype = "f32"),
    tolerance = 1e-5
  )
})
