test_that("errors", {
  check <- function(operand, dtype) {
    expect_snapshot(
      infer_types_bitcast_convert(operand, dtype),
      error = TRUE
    )
  }
  # from boolean not supported
  check(vt("pred", c(2L, 3L)), "i8")
  # to boolean not supported
  check(vt("i8", c(2L, 3L)), "i1")
  # unsupported dtype
  check(vt("i8", c(2L, 3L)), "foo")
  # scalar operand cannot upcast
  check(vt("i8", integer()), "i32")
  # last dimension must match ratio for upcast
  check(vt("i8", c(2L, 3L)), "i32")
})

test_that("basic tests", {
  # Upcasting 1: i1 -> ui8
  # throws error since last dimension should be 4, but is 8
  local_func()
  x <- hlo_input("x", "i8", shape = c(2L, 3L, 8))
  expect_error(
    y <- hlo_bitcast_convert(
      x,
      dtype = "ui32"
    )
  )

  # upcasting 1-d tensor
  local_func()
  x <- hlo_input("x", "i8", shape = c(2L))
  y <- hlo_bitcast_convert(
    x,
    dtype = "i16"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  input <- array(c(1, 10), dim = c(2))
  output <- pjrt_execute(
    exec,
    pjrt_buffer(input, dtype = "i8")
  )
  expect_equal(as_array(output), 2561)

  # upcasting 0-dimensional tensor -> error
  local_func()
  x <- hlo_input("x", "i16")
  expect_error(
    y <- hlo_bitcast_convert(
      x,
      dtype = "i32"
    )
  )

  # downcasting 0-dimensional tensor
  local_func()
  x <- hlo_input("x", "i16")
  y <- hlo_bitcast_convert(
    x,
    dtype = "i8"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  input <- 2561L
  output <- pjrt_execute(
    exec,
    pjrt_scalar(input, dtype = "i16")
  )
  expect_equal(shape(output), c(2))
  expect_equal(as_array(output), array(c(1, 10)))

  # from booleans -> error
  local_func()
  x <- hlo_input("x", "i1", shape = c(2L, 3L, 16L))
  expect_error(
    y <- hlo_bitcast_convert(
      x,
      dtype = "i16"
    )
  )

  # to booleans -> error
  local_func()
  x <- hlo_input("x", "i16", shape = c(2L, 3L))
  expect_error(
    y <- hlo_bitcast_convert(
      x,
      dtype = "pred"
    )
  )

  # Upcasting 2: i8 -> ui32
  local_func()
  x <- hlo_input("x", "i8", shape = c(2L, 3L, 4L))
  y <- hlo_bitcast_convert(
    x,
    dtype = "ui32"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  input <- array(as.integer(seq_len(24)), dim = c(2, 3, 4))
  output <- pjrt_execute(
    exec,
    pjrt_buffer(input, dtype = "i8")
  )
  expect_equal(dim(as_array(output)), c(2, 3))
  expect_true(all(as_array(output) >= 0))

  # identitiy casting ui32 -> f32
  local_func()
  x <- hlo_input("x", "ui32", shape = c(2L, 3L))
  y <- hlo_bitcast_convert(
    x,
    dtype = "f32"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  input <- withr::with_seed(1, array(sample.int(1000, 6), dim = c(2, 3)))
  output <- pjrt_execute(
    exec,
    pjrt_buffer(input, dtype = "ui32")
  )
  expect_equal(dim(as_array(output)), c(2, 3))
  expect_true(all(as_array(output) <= 1e-41))

  # downcasting: f64 -> f32
  local_func()
  x <- hlo_input("x", "f64", shape = c(2L, 3L))
  y <- hlo_bitcast_convert(
    x,
    dtype = "f32"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  input <- array(rnorm(6), dim = c(2, 3))
  output <- pjrt_execute(
    exec,
    pjrt_buffer(input, dtype = "f64")
  )
  expect_equal(dim(as_array(output)), c(2, 3, 2))
})
