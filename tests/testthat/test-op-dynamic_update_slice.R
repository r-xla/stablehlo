test_that("basic tests", {
  local_func()

  x <- hlo_input("x", "f32", shape = c(4L, 4L))
  upd <- hlo_input("update", "f32", shape = c(2L, 2L))

  y <- hlo_dynamic_update_slice(
    x,
    upd,
    start_indices = c(-1L, 3L)
  )
  f <- hlo_return(y)

  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  operand <- matrix(
    as.numeric(c(
      1,
      1,
      0,
      0,
      1,
      1,
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1
    )),
    nrow = 4,
    byrow = TRUE
  )
  update <- array(1, dim = c(2, 2))

  # According to SPEC, start indices are clamped, so result should be all 1s
  expected <- array(1, dim = c(4, 4))

  out <- pjrt_execute(exec, pjrt_buffer(operand), pjrt_buffer(update))
  expect_equal(as_array(out), expected)
})
