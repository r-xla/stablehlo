test_that("basic edge padding", {
  local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0L, dtype = "i32")
  y <- hlo_pad(
    x,
    pad_val,
    edge_padding_low = c(0, 1),
    edge_padding_high = c(2, 1),
    interior_padding = c(0, 0)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.integer(1:6), dim = c(2, 3))

  # edge_padding_low = c(0, 1) adds 1 column on the left

  # edge_padding_high = c(2, 1) adds 2 rows at the bottom and 1 column on the right
  # Result shape: (2+0+2, 3+1+1) = (4, 5)
  expected <- array(0L, dim = c(4, 5))
  expected[1:2, 2:4] <- input

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("interior padding", {
  local_func()
  x <- hlo_input("x", "i32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0L, dtype = "i32")
  y <- hlo_pad(
    x,
    pad_val,
    edge_padding_low = c(0, 1),
    edge_padding_high = c(2, 1),
    interior_padding = c(1, 2)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.integer(1:6), dim = c(2, 3))

  # interior_padding = c(1, 2) adds 1 row between rows and 2 columns between columns
  # Result shape: (2 + 0 + max(2-1,0)*1 + 2, 3 + 1 + max(3-1,0)*2 + 1) = (5, 9)
  expected <- array(0L, dim = c(5, 9))
  # Original values placed at:
  # row indices: 1, 3 (stride of 2 due to interior padding of 1)
  # col indices: 2, 5, 8 (offset by 1 due to edge_padding_low, stride of 3 due to interior padding of 2)
  expected[1, 2] <- 1L
  expected[3, 2] <- 2L
  expected[1, 5] <- 3L
  expected[3, 5] <- 4L
  expected[1, 8] <- 5L
  expected[3, 8] <- 6L

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("float padding", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(3L, 2L))
  pad_val <- hlo_scalar(-1.0, dtype = "f32")
  y <- hlo_pad(
    x,
    pad_val,
    edge_padding_low = c(1, 1),
    edge_padding_high = c(1, 1),
    interior_padding = c(0, 0)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.numeric(1:6), dim = c(3, 2))

  # Simple padding: add 1 row/col on each side
  # Result shape: (3+1+1, 2+1+1) = (5, 4)
  expected <- array(-1.0, dim = c(5, 4))
  expected[2:4, 2:3] <- input

  output <- pjrt_execute(
    exec,
    pjrt_buffer(input)
  )
  expect_equal(as_array(output), expected)
})

test_that("type mismatch error", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0L, dtype = "i32")

  expect_error(
    hlo_pad(
      x,
      pad_val,
      edge_padding_low = c(0, 0),
      edge_padding_high = c(0, 0),
      interior_padding = c(0, 0)
    ),
    "same dtype"
  )
})

test_that("negative interior padding error", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  pad_val <- hlo_scalar(0.0, dtype = "f32")

  expect_error(
    hlo_pad(
      x,
      pad_val,
      edge_padding_low = c(0, 0),
      edge_padding_high = c(0, 0),
      interior_padding = c(-1, 0)
    ),
    "non-negative"
  )
})
