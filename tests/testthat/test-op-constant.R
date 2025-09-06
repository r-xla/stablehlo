test_that("scalars", {
  check <- function(x, dtype = NULL) {
    f <- if (!is.null(dtype)) {
      hlo_scalar(x, dtype)
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

test_that("errors scalars", {
  expect_error(hlo_scalar(3L, "f32"), "Invalid dtype for integer")
  expect_error(hlo_scalar(1, "i32"), "Invalid dtype for double")
  expect_error(hlo_scalar(1:2), "a single value")
})

test_that("specify shape in hlo_tensor", {
  expect_snapshot(repr(hlo_tensor(1:2, shape = c(2, 1))@func))
  expect_snapshot(repr(hlo_tensor(1:2)@func))
  expect_snapshot(repr(hlo_tensor(1)@func))
})

test_that("compiles", {
  f <- hlo_return(hlo_scalar(1L, "i32"))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  expect_equal(
    pjrt_execute(executable),
    pjrt_scalar(1L, "i32")
  )

  # same for tensor
  f <- hlo_return(hlo_tensor(1:2, shape = c(2, 1)))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  expect_class(program, "PJRTProgram")
  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")
  expect_equal(
    pjrt_execute(executable),
    pjrt_buffer(1:2, shape = c(2, 1))
  )
})
