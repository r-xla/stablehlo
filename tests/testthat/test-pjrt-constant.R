skip_if_not_installed("pjrt")

test_that("scalars are correctly formatted", {
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

  # [0000 0000] ... [0000 0001]
  # 0x00        ... 0x80 (16 * 8 = 128 = 2^7 = 0000 0001)
  smallest_i64_raw <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80))
  check_roundtrip_scalar(pjrt_scalar(smallest_i64_raw, dtype = "i64"))

  # [1111 1111] ... [1111 1110]
  largest_i64_raw <- as.raw(c(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f))
  check_roundtrip_scalar(pjrt_scalar(largest_i64_raw, dtype = "i64"))

  # [1111 1111] ... [1111 1111]
  largest_u64_raw <- as.raw(rep(0xff, 8))
  check_roundtrip_scalar(pjrt_scalar(largest_u64_raw, dtype = "ui64"))
})

test_that("tensors are correctly formatted", {
  check_roundtrip <- function(input_buffer) {
    local_func()
    f <- hlo_return(hlo_tensor(input_buffer))

    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    result_buffer <- pjrt_execute(exec)

    expect_equal(result_buffer, input_buffer)
  }

  # 2D tensor
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i32"))
  check_roundtrip(pjrt_buffer(array(rnorm(6), dim = c(2, 3)), dtype = "f32"))
  check_roundtrip(pjrt_buffer(array(rnorm(6), dim = c(2, 3)), dtype = "f64"))
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i64"))
  check_roundtrip(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "ui32"))
  check_roundtrip(pjrt_buffer(
    array(c(TRUE, FALSE, TRUE, TRUE), dim = c(2, 2)),
    dtype = "pred"
  ))
  check_roundtrip(pjrt_buffer(array(1:4, dim = c(2, 2)), dtype = "i8"))
  check_roundtrip(pjrt_buffer(array(1:4, dim = c(2, 2)), dtype = "ui8"))

  # 3D tensor
  check_roundtrip(pjrt_buffer(array(1:24, dim = c(2, 3, 4)), dtype = "i32"))
  check_roundtrip(pjrt_buffer(
    array(rnorm(24), dim = c(2, 3, 4)),
    dtype = "f32"
  ))

  # 0-dimensional tensor (empty tensors)
  check_roundtrip(pjrt_buffer(integer(0), shape = c(0L, 2L, 3L), dtype = "i32"))
  check_roundtrip(pjrt_buffer(numeric(0), shape = c(2L, 0L, 3L), dtype = "f32"))
})

test_that("0-dimensional buffers", {
  check_roundtrip <- function(input_buffer) {
    local_func()
    f <- hlo_return(hlo_tensor(input_buffer))
    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    result_buffer <- pjrt_execute(exec)
    expect_equal(result_buffer, input_buffer)
  }
  check_roundtrip(pjrt_buffer(integer(0), shape = c(0L, 2L, 3L), dtype = "i32"))
  check_roundtrip(pjrt_buffer(numeric(0), shape = c(2L, 0L, 3L), dtype = "f32"))
  check_roundtrip(pjrt_buffer(numeric(0), shape = c(0L, 0L), dtype = "f32"))
})

test_that("wrong input type", {
  local_func()
  expect_error(
    hlo_scalar(pjrt_buffer(array(1:6, dim = c(2, 3)), dtype = "i32")),
    "expects a scalar"
  )
})
