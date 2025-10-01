test_that("rng_bit_generator three_fry", {
  func <- local_func()
  initial_state <- hlo_tensor(c(1L, 2L), dtype = "ui64", shape = 2L)
  result <- hlo_rng_bit_generator("THREE_FRY", initial_state)
  expect_length(result, 2)
  expect_snapshot(repr(result[[1]]@func@body@items[[2]]))
})

test_that("rng_bit_generator philox", {
  func <- local_func()
  initial_state <- hlo_tensor(c(1L, 2L, 3L), dtype = "ui64", shape = 3L)
  result <- hlo_rng_bit_generator("PHILOX", initial_state)
  expect_length(result, 2)
  expect_snapshot(repr(result[[1]]@func@body@items[[2]]))
})

test_that("rng_bit_generator default", {
  func <- local_func()
  initial_state <- hlo_tensor(c(1L, 2L), dtype = "ui64", shape = 2L)
  result <- hlo_rng_bit_generator("DEFAULT", initial_state)
  expect_length(result, 2)
  expect_snapshot(repr(result[[1]]@func@body@items[[2]]))
})

test_that("rng_bit_generator error handling", {
  func <- local_func()

  # Invalid algorithm
  initial_state <- hlo_tensor(c(1L, 2L), dtype = "ui64", shape = 2L)
  expect_error(
    hlo_rng_bit_generator("INVALID", initial_state),
    "rng_algorithm must be one of: 'DEFAULT', 'THREE_FRY', 'PHILOX'"
  )

  # Wrong state type
  wrong_type_state <- hlo_tensor(c(1L, 2L), dtype = "i64", shape = 2L)
  expect_error(
    hlo_rng_bit_generator("THREE_FRY", wrong_type_state),
    "'initial_state' must be of type ui64"
  )

  # Non-1D state
  wrong_shape_state <- hlo_tensor(array(c(1L, 2L), dim = c(1, 2)), dtype = "ui64", shape = c(1L, 2L))
  expect_error(
    hlo_rng_bit_generator("THREE_FRY", wrong_shape_state),
    "'initial_state' must be a 1-dimensional tensor"
  )

  # THREE_FRY with wrong size
  wrong_size_state <- hlo_tensor(c(1L, 2L, 3L), dtype = "ui64", shape = 3L)
  expect_error(
    hlo_rng_bit_generator("THREE_FRY", wrong_size_state),
    "For THREE_FRY algorithm, initial_state must have size 2"
  )

  # PHILOX with wrong size
  wrong_size_state_philox <- hlo_tensor(c(1L), dtype = "ui64", shape = 1L)
  expect_error(
    hlo_rng_bit_generator("PHILOX", wrong_size_state_philox),
    "For PHILOX algorithm, initial_state must have size 2 or 3"
  )

  # PHILOX with size 4 (invalid)
  wrong_size_state_philox2 <- hlo_tensor(c(1L, 2L, 3L, 4L), dtype = "ui64", shape = 4L)
  expect_error(
    hlo_rng_bit_generator("PHILOX", wrong_size_state_philox2),
    "For PHILOX algorithm, initial_state must have size 2 or 3"
  )
})
