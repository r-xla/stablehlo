test_that("basic tests", {
  func <- local_func()
  x1 <- hlo_input("x1", "i32", shape = c(3L, 1L))
  x2 <- hlo_input("x2", "i32", shape = c(3L, 2L))
  x3 <- hlo_input("x3", "i32", shape = c(3L, 3L))
  y <- hlo_concatenate(
    x1,
    x2,
    x3,
    dimension = 1L
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

test_that("works with 3D tensors", {
  out <- infer_types_concatenate(
    ValueType(TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 3, 4)))),
    ValueType(TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 1, 4)))),
    dimension = Constant(
      1L,
      TensorType(dtype = as_dtype("i64"), shape = Shape(integer()))
    )
  )[[1L]]$type
  expect_equal(
    out,
    TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 4, 4)))
  )
})

test_that("errors", {
  # (C3) no inputs
  expect_snapshot(
    infer_types_concatenate(dimension = scnst(0L, "i64")),
    error = TRUE
  )
  # (C1) different data types
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      vt("i32", c(2L, 3L)),
      dimension = scnst(0L, "i64")
    ),
    error = TRUE
  )
  # (C4) dimension out of bounds
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      dimension = scnst(2L, "i64")
    ),
    error = TRUE
  )
  # (C2) non-concat dimension shape mismatch
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      vt("f32", c(2L, 4L)),
      dimension = scnst(0L, "i64")
    ),
    error = TRUE
  )
})

test_that("a negative dimension is rejected", {
  # (C4) is `0 <= dimension < rank`. Unguarded on the low side, `dim_r` goes
  # negative and `x[-dim_r]` flips from dropping that axis to keeping only it,
  # so a wrong result type came out with no error at all.
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      vt("f32", c(2L, 3L)),
      dimension = scnst(-2L, "i64")
    ),
    error = TRUE
  )
})

test_that("no inputs reports concatenate's own error", {
  # (C3) `0 < N`. The op has no value operands to take a func from, and used to
  # die with `subscript out of bounds` before reaching this check.
  local_func()
  expect_error(hlo_concatenate(dimension = 0L), "at least one input")
})

test_that("inputs of different rank are rejected", {
  # (C2) `same(shape(inputs...))` covers the rank too. `x[-dim_r]` drops
  # nothing from an input of lower rank, so the shape check passed and the
  # result came out with an `NA` size along the concatenation dimension.
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L, 4L)),
      vt("f32", c(2L, 3L)),
      dimension = scnst(2L, "i64")
    ),
    error = TRUE
  )
})
