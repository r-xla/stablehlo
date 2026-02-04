test_that("basic tests", {
  local_func()

  # Test from the spec
  operand <- hlo_input("operand", "i32", shape = c(4L, 4L))
  update <- hlo_input("update", "i32", shape = c(2L, 2L))
  start0 <- hlo_scalar(-1L, dtype = "i64")
  start1 <- hlo_scalar(3L, dtype = "i64")

  y <- hlo_dynamic_update_slice(
    operand,
    update,
    start0,
    start1
  )

  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  operand_input <- array(
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
    dim = c(4L, 4L)
  )

  update_input <- array(c(1L, 2L, 3L, 4L), dim = c(2L, 2L))

  expected <- array(
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 3L, 4L, 1L, 1L),
    dim = c(4L, 4L)
  )

  output <- pjrt_execute(
    exec,
    pjrt_buffer(operand_input),
    pjrt_buffer(update_input)
  )
  expect_equal(as_array(output), expected)
})

test_that("errors", {
  # start_indices must be 0-dimensional
  expect_snapshot(
    infer_types_dynamic_update_slice(
      vt("f32", c(4L, 5L)),
      vt("f32", c(2L, 3L)),
      vt("i32", 2L),
      vt("i32", integer())
    ),
    error = TRUE
  )
  # (C3) rank mismatch
  expect_snapshot(
    infer_types_dynamic_update_slice(
      vt("f32", c(4L, 5L)),
      vt("f32", c(2L, 3L, 1L)),
      vt("i32", integer()),
      vt("i32", integer())
    ),
    error = TRUE
  )
  # (C4) wrong number of start_indices
  expect_snapshot(
    infer_types_dynamic_update_slice(
      vt("f32", c(4L, 5L)),
      vt("f32", c(2L, 3L)),
      vt("i32", integer())
    ),
    error = TRUE
  )
  # (C6) update shape exceeds operand
  expect_snapshot(
    infer_types_dynamic_update_slice(
      vt("f32", c(4L, 5L)),
      vt("f32", c(5L, 3L)),
      vt("i32", integer()),
      vt("i32", integer())
    ),
    error = TRUE
  )
})
