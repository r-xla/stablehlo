test_that("scalars", {
  check <- function(x, dtype = NULL) {
    f <- if (!is.null(dtype)) {
      hlo_scalar(x, dtype = dtype)
    } else {
      hlo_scalar(x)
    }
    expect_snapshot(repr(f@func@body@items[[1]]))
  }
  check(3.14, "f32")
  check(3.14, "f64")
  check(3L, "i32")
  check(3L, "i64")
  check(3L, "i16")
  check(3L, "ui32")
  check(3L, "ui64")
  check(3L, "ui16")
  check(-3L)
  check(-100L)
  check(TRUE)
  check(FALSE)
})

test_that("compile scalars", {
  skip_if_not_installed("pjrt")
  check <- function(x, dtype) {
    f <- hlo_return(hlo_scalar(x, dtype = dtype))
    f@id <- FuncId("main")
    program <- pjrt::pjrt_program(repr(f))
    exec <- pjrt::pjrt_compile(program)
    buffer <- pjrt::pjrt_execute(exec)
    expect_equal(
      pjrt::as_array(buffer),
      x,
      tolerance = if (startsWith(dtype, "f")) 1e-6 else 0
    )
  }
  check(3.14, "f32")
  check(-8.23, "f32")
  check(3L, "i32")
  check(3L, "i64")
  check(3L, "i16")
  check(3L, "ui32")
  check(3L, "ui16")
  check(TRUE, "pred")
  check(FALSE, "pred")

  skip_if_metal("types not supported on Metal")

  check(3L, "ui64")
  check(-8.23, "f64")
  check(3.14, "f64")
})

test_that("arrays", {
  expect_snapshot(repr(hlo_tensor(array(1:2))@func))
  expect_snapshot(repr(hlo_tensor(array(1:6, dim = c(2, 3)))@func))
  expect_snapshot(repr(hlo_tensor(array(1:6, dim = c(2, 3, 1)))@func))
})

test_that("compile tensors", {
  skip_if_not_installed("pjrt")
  check <- function(x, dtype) {
    f <- hlo_return(hlo_tensor(x, dtype = dtype))
    f@id <- FuncId("main")
    program <- pjrt::pjrt_program(repr(f))
    exec <- pjrt::pjrt_compile(program)
    buffer <- pjrt::pjrt_execute(exec)
    expect_equal(
      buffer,
      pjrt::pjrt_buffer(x)
    )
  }
  check(array(1:2), "i32")
  check(array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3)), "f32")
  check(array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3, 1)), "f32")

  check(1:2, "i32")
  check(c(1, 2, 3), "f32")
  check(c(TRUE, FALSE), "pred")
})


test_that("errors", {
  expect_error(hlo_scalar(-1L, dtype = "ui16"), "must be non-negative")
  expect_error(hlo_scalar(NA), "must not contain NA")
  expect_error(hlo_scalar(3L, dtype = "f32"), "Invalid dtype for integer")
  expect_error(hlo_scalar(1, dtype = "i32"), "Invalid dtype for double")
  expect_error(hlo_scalar(1:2), "a single value")
})

test_that("specify shape in hlo_tensor", {
  expect_snapshot(repr(hlo_tensor(1:2, shape = c(2, 1))@func))
  expect_snapshot(repr(hlo_tensor(1:2)@func))
  expect_snapshot(repr(hlo_tensor(1)@func))
})

test_that("PJRTBuffer", {
  skip_if_not_installed("pjrt")
  expect_snapshot(repr(hlo_tensor(pjrt::pjrt_buffer(1), dtype = "i32")@func))
  expect_snapshot(repr(hlo_scalar(pjrt::pjrt_scalar(1), dtype = "i32")@func))
})
