test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L, 2L))
  y <- hlo_reshape(
    x,
    shape = c(4L, 3L)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.double(1:12), dim = c(2, 3, 2))

  expected <- aperm(array(aperm(input, c(3, 2, 1)), dim = c(3, 4)), c(2, 1))

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})

test_that("errors", {
  # (C2) size mismatch
  expect_snapshot(
    infer_types_reshape(vt("f32", c(2L, 3L)), shape = c(4L, 2L)),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("reshape defers the element-count check when either side is dynamic", {
  # Both known and unequal: still refused.
  local_func()
  expect_error(
    hlo_reshape(dyn_input("a", "f32", c(2L, 3L)), shape = c(4L, 2L)),
    "Size of output must equal"
  )
  # Dynamic operand: whether the counts match is a run-time question.
  expect_equal(
    inferred(function() {
      hlo_reshape(dyn_input("a", "f32", c(N, 3L)), shape = c(6L, 1L))
    }),
    "tensor<6x1xf32>"
  )
})
