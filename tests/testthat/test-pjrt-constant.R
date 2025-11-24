test_that("PJRTBuffer with hlo_tensor roundtrip", {
  skip_if_not_installed("pjrt")

  check_roundtrip <- function(input_buffer) {
    local_func()
    # Create a simple function that takes no arguments and returns the constant
    tensor <- hlo_tensor(input_buffer)
    f <- hlo_return(tensor)

    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    result_buffer <- pjrt_execute(exec)

    # Compare input and output buffers
    expect_equal(result_buffer, input_buffer)
  }

  # Test with various types and shapes

  # Int32
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i32"))

  # Float32
  check_roundtrip(pjrt_buffer(array(rnorm(6), dim = c(2, 3)), dtype = "f32"))

  # Float64
  check_roundtrip(pjrt_buffer(array(rnorm(6), dim = c(2, 3)), dtype = "f64"))

  # Int64 (using integer, so limited range but checks type handling)
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i64"))

  # Uint32
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "ui32"))

  # Boolean/Pred
  check_roundtrip(pjrt_buffer(
    array(c(TRUE, FALSE, TRUE, TRUE), dim = c(2, 2)),
    dtype = "pred"
  ))

  # Int8
  check_roundtrip(pjrt_buffer(array(1:4, dim = c(2, 2)), dtype = "i8"))

  # Scalars (rank 0)
  check_roundtrip(pjrt_scalar(42L, dtype = "i32"))
  check_roundtrip(pjrt_scalar(3.14, dtype = "f32"))

  # 3D Array
  check_roundtrip(pjrt_buffer(array(1:24, dim = c(2, 3, 4)), dtype = "i32"))
  check_roundtrip(pjrt_buffer(
    array(rnorm(24), dim = c(2, 3, 4)),
    dtype = "f32"
  ))

  # Large integers (2^33)
  # Create raw representation of 2^33 (8589934592)
  # In hex: 0x0000000200000000
  # Little endian: 00 00 00 00 02 00 00 00
  large_int_raw <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00))
  check_roundtrip(pjrt_buffer(
    large_int_raw,
    dtype = "i64",
    shape = 1L,
    row_major = TRUE
  ))

  # Large unsigned integers
  check_roundtrip(pjrt_buffer(
    large_int_raw,
    dtype = "ui64",
    shape = 1L,
    row_major = TRUE
  ))

  # Large integer in tensor
  # Two elements: 2^33 and 1
  large_tensor_raw <- c(
    large_int_raw,
    as.raw(c(0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))
  )
  check_roundtrip(pjrt_buffer(
    large_tensor_raw,
    dtype = "i64",
    shape = 2L,
    row_major = TRUE
  ))
})

test_that("PJRTBuffer with hlo_scalar roundtrip", {
  skip_if_not_installed("pjrt")

  check_roundtrip_scalar <- function(input_buffer) {
    local_func()
    scalar <- hlo_scalar(input_buffer)
    f <- hlo_return(scalar)

    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    result_buffer <- pjrt_execute(exec)

    expect_equal(result_buffer, input_buffer)
  }

  check_roundtrip_scalar(pjrt_scalar(123L, dtype = "i32"))
  check_roundtrip_scalar(pjrt_scalar(1.23, dtype = "f32"))
  check_roundtrip_scalar(pjrt_scalar(TRUE, dtype = "pred"))

  # Large integer scalar
  large_int_raw <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00))
  check_roundtrip_scalar(pjrt_scalar(large_int_raw, dtype = "i64"))

  # Large unsigned integer scalar
  check_roundtrip_scalar(pjrt_scalar(large_int_raw, dtype = "ui64"))
})

test_that("snapshots", {
  skip_if_not_installed("pjrt")
  local_func()

  # Rank 2 tensor
  x <- pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i32")
  tensor <- hlo_tensor(x)
  expect_snapshot(repr(tensor@func))

  # 3D Tensor
  x_3d <- pjrt_buffer(array(1:24, dim = c(2, 3, 4)), dtype = "i32")
  tensor_3d <- hlo_tensor(x_3d)
  expect_snapshot(repr(tensor_3d@func))

  # Scalar
  s <- pjrt_scalar(42L, dtype = "i32")
  scalar <- hlo_scalar(s)
  expect_snapshot(repr(scalar@func))

  # Large integer
  large_int_raw <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00))
  large_s <- pjrt_scalar(large_int_raw, dtype = "i64")
  large_scalar <- hlo_scalar(large_s)
  expect_snapshot(repr(large_scalar@func))
})
