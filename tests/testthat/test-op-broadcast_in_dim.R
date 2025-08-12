test_that("basic broadcast", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(1L, 3L), func_id = "main")
  y <- hlo_broadcast_in_dim(x, broadcast_dimensions = c(0L, 2L), shape_out = c(2L, 1L, 3L))
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.double(c(1, 2, 3)), dim = c(1, 3))
  expected <- array(0, dim = c(2, 1, 3))
  expected[1, 1, ] <- input
  expected[2, 1, ] <- input

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})
