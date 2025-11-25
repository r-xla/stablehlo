test_that("basic tests", {
  # Upcasting 1: i1 -> ui8
  # throws error since last dimension should be 8, but is 16
  local_func()
  x <- hlo_input("x", "i1", shape = c(2L, 3L, 16L))
  expect_error(
    y <- hlo_bitcast_convert(
      x,
      dtype = "ui8"
    )
  )

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
