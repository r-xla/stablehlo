test_that("FFT (complex -> complex) repr", {
  local_func()
  x <- hlo_input("x", "c64", shape = c(2L, 4L))
  y <- hlo_fft(x, "FFT", fft_length = 4L)
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("IFFT (complex -> complex) repr", {
  local_func()
  x <- hlo_input("x", "c128", shape = c(3L, 4L, 8L))
  y <- hlo_fft(x, "IFFT", fft_length = c(4L, 8L))
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("RFFT (real -> complex) repr and shape", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 4L))
  y <- hlo_fft(x, "RFFT", fft_length = 4L)
  expect_equal(shape(y), c(2L, 3L))
  expect_equal(dtype(y), ComplexType(32L))
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("IRFFT (complex -> real) repr and shape", {
  local_func()
  x <- hlo_input("x", "c64", shape = c(2L, 3L))
  y <- hlo_fft(x, "IRFFT", fft_length = 4L)
  expect_equal(shape(y), c(2L, 4L))
  expect_equal(dtype(y), FloatType(32L))
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("RFFT -> IRFFT round-trip executes", {
  skip_if_not_installed("pjrt")
  local_func()
  x <- hlo_input("x", "f32", shape = 8L)
  y <- hlo_fft(x, "RFFT", fft_length = 8L)
  z <- hlo_fft(y, "IRFFT", fft_length = 8L)
  f <- hlo_return(z)

  program <- pjrt::pjrt_program(repr(f))
  exe <- pjrt::pjrt_compile(program)
  input <- array(as.numeric(seq_len(8)), dim = 8L)
  inp_buf <- pjrt::pjrt_buffer(input, dtype = "f32")
  out_buf <- pjrt::pjrt_execute(exe, inp_buf)
  expect_equal(shape(out_buf), 8L)
  expect_equal(pjrt::as_array(out_buf), input, tolerance = 1e-4)
})

test_that("multi-dim RFFT collapses last dim", {
  local_func()
  x <- hlo_input("x", "f64", shape = c(3L, 4L, 8L))
  y <- hlo_fft(x, "RFFT", fft_length = c(4L, 8L))
  expect_equal(shape(y), c(3L, 4L, 5L))
  expect_equal(dtype(y), ComplexType(64L))
})

test_that("errors", {
  # invalid fft_type (C2)
  expect_snapshot(
    infer_types_fft(
      vt("c64", 4L),
      "INVALID",
      cnst(4L, "i64", 1L)
    ),
    error = TRUE
  )

  # FFT requires complex operand (C2)
  expect_snapshot(
    infer_types_fft(
      vt("f32", 4L),
      "FFT",
      cnst(4L, "i64", 1L)
    ),
    error = TRUE
  )

  # RFFT requires float operand (C2)
  expect_snapshot(
    infer_types_fft(
      vt("c64", 4L),
      "RFFT",
      cnst(4L, "i64", 1L)
    ),
    error = TRUE
  )

  # IRFFT requires complex operand (C2)
  expect_snapshot(
    infer_types_fft(
      vt("f32", 4L),
      "IRFFT",
      cnst(4L, "i64", 1L)
    ),
    error = TRUE
  )

  # fft_length too long (C3)
  expect_snapshot(
    infer_types_fft(
      vt("c64", c(2L, 2L, 2L, 4L)),
      "FFT",
      cnst(c(2L, 2L, 2L, 4L), "i64", 4L)
    ),
    error = TRUE
  )

  # fft_length empty (C3)
  expect_snapshot(
    infer_types_fft(
      vt("c64", 4L),
      "FFT",
      cnst(integer(), "i64", 0L)
    ),
    error = TRUE
  )

  # fft_length longer than rank (C1)
  expect_snapshot(
    infer_types_fft(
      vt("c64", 4L),
      "FFT",
      cnst(c(2L, 4L), "i64", 2L)
    ),
    error = TRUE
  )

  # RFFT operand trailing dims must equal fft_length (C4)
  expect_snapshot(
    infer_types_fft(
      vt("f32", c(2L, 4L)),
      "RFFT",
      cnst(8L, "i64", 1L)
    ),
    error = TRUE
  )

  # IRFFT operand last dim must be fft_length / 2 + 1 (C4, C5)
  expect_snapshot(
    infer_types_fft(
      vt("c64", c(2L, 4L)),
      "IRFFT",
      cnst(8L, "i64", 1L)
    ),
    error = TRUE
  )
})
