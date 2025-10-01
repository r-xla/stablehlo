test_that("scalars", {
  check <- function(x, dtype = NULL) {
    func <- local_func()
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
    func <- local_func()
    f <- hlo_return(hlo_scalar(x, dtype = dtype))
    f@id <- FuncId("main")
    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    buffer <- pjrt_execute(exec)
    expect_equal(
      as_array(buffer),
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
  expect_snapshot(repr(hlo_tensor(array(1:2), func = hlo_func())@func))
  expect_snapshot(repr(
    hlo_tensor(array(1:6, dim = c(2, 3)), func = hlo_func())@func
  ))
  expect_snapshot(repr(
    hlo_tensor(array(1:6, dim = c(2, 3, 1)), func = hlo_func())@func
  ))
})

test_that("compile tensors", {
  skip_if_not_installed("pjrt")
  check <- function(x, dtype) {
    func <- local_func()
    f <- hlo_return(hlo_tensor(x, dtype = dtype))
    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    buffer <- pjrt_execute(exec)
    expect_equal(
      buffer,
      pjrt_buffer(x)
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
  expect_error(
    hlo_scalar(-1L, dtype = "ui16", func = hlo_func()),
    "must be non-negative"
  )
  expect_error(hlo_scalar(NA, func = hlo_func()), "must not contain NA")
  expect_error(
    hlo_scalar(3L, dtype = "f32", func = hlo_func()),
    "Invalid dtype for integer"
  )
  expect_error(
    hlo_scalar(1, dtype = "i32", func = hlo_func()),
    "Invalid dtype for double"
  )
  expect_error(hlo_scalar(1:2, func = hlo_func()), "a single value")
})

test_that("specify shape in hlo_tensor", {
  func <- local_func()
  expect_snapshot(repr(
    hlo_tensor(1:2, shape = c(2, 1), func = hlo_func())@func
  ))
  expect_snapshot(repr(hlo_tensor(1:2, func = hlo_func())@func))
  expect_snapshot(repr(hlo_tensor(1, func = hlo_func())@func))
})

test_that("PJRTBuffer", {
  skip_if_not_installed("pjrt")
  func <- local_func()
  expect_snapshot(repr(
    hlo_tensor(pjrt_buffer(1), dtype = "i32", func = hlo_func())@func
  ))
  expect_snapshot(repr(
    hlo_scalar(pjrt_scalar(1), dtype = "i32", func = hlo_func())@func
  ))
})

test_that("empty array formatting", {
  func <- local_func()

  # Test empty array with simplify_dense=TRUE (default)
  empty_tensor <- hlo_tensor(integer(), dtype = "i64", func = func)
  constant_op <- empty_tensor@func@body@items[[1]]
  expect_snapshot(repr(constant_op@inputs@attrs@items[[1]]@value@value, simplify_dense = TRUE))

  # Test empty array with simplify_dense=FALSE
  expect_snapshot(repr(constant_op@inputs@attrs@items[[1]]@value@value, simplify_dense = FALSE))

  # Test empty array with different dtypes
  empty_f32 <- hlo_tensor(numeric(), dtype = "f32", func = func)
  constant_op_f32 <- empty_f32@func@body@items[[1]]
  expect_snapshot(repr(constant_op_f32@inputs@attrs@items[[1]]@value@value, simplify_dense = TRUE))

  empty_bool <- hlo_tensor(logical(), dtype = "pred", func = func)
  constant_op_bool <- empty_bool@func@body@items[[1]]
  expect_snapshot(repr(constant_op_bool@inputs@attrs@items[[1]]@value@value, simplify_dense = TRUE))
})

test_that("empty array execution", {
  skip_if_not_installed("pjrt")
  func <- local_func()
  empty_tensor <- hlo_empty("i64", 0L)
  constant_op <- empty_tensor@func@body@items[[1]]
  f <- hlo_return(empty_tensor)
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec)
  expect_equal(as_array(buffer), integer())
})
