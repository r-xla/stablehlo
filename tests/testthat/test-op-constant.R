test_that("scalars", {
  check <- function(x, dtype = NULL) {
    func <- local_func()
    f <- if (!is.null(dtype)) {
      hlo_scalar(x, dtype = dtype)
    } else {
      hlo_scalar(x)
    }
    expect_snapshot(repr(f$func$body[[1]]))
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
  check <- function(x, dtype = NULL) {
    func <- local_func()
    y <- if (is.null(dtype)) {
      hlo_scalar(x)
    } else {
      hlo_scalar(x, dtype = dtype)
    }
    f <- hlo_return(y)
    f$id <- FuncId("main")
    program <- pjrt_program(repr(f))
    exec <- pjrt_compile(program)
    buffer <- pjrt_execute(exec)
    expect_equal(
      as_array(buffer),
      x,
      tolerance = if (is.null(dtype) || startsWith(dtype, "f")) 1e-6 else 0
    )
  }
  check(3.14, "f32")
  check(-8.23, "f32")
  check(3L, "i32")
  check(3L, "i64")
  check(3L, "i16")
  check(3L, "ui32")
  check(3L, "ui16")
  check(TRUE)
  check(FALSE)
  check(3L, "ui64")
  check(-8.23, "f64")
  check(3.14, "f64")
})

test_that("arrays", {
  expect_snapshot(repr(hlo_tensor(array(1:2), func = hlo_func())$func))
  expect_snapshot(repr(
    hlo_tensor(array(1:6, dim = c(2, 3)), func = hlo_func())$func
  ))
  expect_snapshot(repr(
    hlo_tensor(array(1:6, dim = c(2, 3, 1)), func = hlo_func())$func
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

test_that("pjrt int from R double", {
  skip_if_not_installed("pjrt")
  func <- local_func()
  f <- hlo_return(hlo_scalar(3.14, dtype = "i32"))
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec)
  expect_equal(as_array(buffer), 3L)
})

test_that("pjrt float from R int", {
  skip_if_not_installed("pjrt")
  func <- local_func()
  f <- hlo_return(hlo_scalar(3L, dtype = "f32"))
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec)
  expect_equal(as_array(buffer), 3.0)
})

test_that("errors", {
  expect_error(
    hlo_scalar(-1L, dtype = "ui16", func = hlo_func()),
    "must be non-negative"
  )
  expect_error(hlo_scalar(1:2, func = hlo_func()), "a single value")
})

test_that("clean error when no current function exists", {
  # Ensure no function is active
  globals$CURRENT_FUNC <- NULL
  expect_error(hlo_scalar(1), "No function is currently being built")
})

test_that("specify shape in hlo_tensor", {
  local_func()
  expect_snapshot(repr(
    hlo_tensor(1:2, shape = c(2, 1), func = hlo_func())$func
  ))
  expect_snapshot(repr(hlo_tensor(1:2, func = hlo_func())$func))
  expect_snapshot(repr(hlo_tensor(1, func = hlo_func())$func))
})

test_that("PJRTBuffer", {
  skip_if_not_installed("pjrt")
  local_func()
  expect_snapshot(repr(
    hlo_tensor(pjrt_buffer(1L, dtype = "i32"), func = hlo_func())$func
  ))
  expect_snapshot(repr(
    hlo_scalar(pjrt_scalar(1, dtype = "f32"), func = hlo_func())$func
  ))
})

test_that("empty array: dense<[]> formatting", {
  skip_if_not_installed("pjrt")
  local_func()
  empty_tensor <- hlo_empty("i64", 0L)
  constant_op <- empty_tensor$func$body[[1]]
  f <- hlo_return(empty_tensor)
  expect_snapshot(repr(f))
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec)
  expect_equal(as_array(buffer), array(integer(), 0L))
})

test_that("empty array: array<> formatting", {
  local_func()
  empty_tensor <- hlo_input("x", "i64", shape = integer())
  y <- hlo_transpose(empty_tensor, permutation = integer())
  f <- hlo_return(y)
  expect_snapshot(repr(f))
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec, pjrt_scalar(1L, "i64"))
  expect_equal(buffer, pjrt_scalar(1L, "i64"))
})

test_that("scalar constant with hlo_tensor", {
  local_func()
  expect_snapshot(repr(hlo_tensor(1L, shape = integer())$func))
})

test_that("can use dtype with constant", {
  local_func()
  expect_snapshot(hlo_scalar(FALSE, BooleanType()))
})

test_that("nan, inf, -inf", {
  skip_if_not_installed("pjrt")
  local_func()
  x1 <- hlo_scalar(Inf, dtype = "f32")
  x2 <- hlo_scalar(-Inf, dtype = "f32")
  x3 <- hlo_scalar(NaN, dtype = "f32")
  f <- hlo_return(x1, x2, x3)
  exec <- pjrt_compile(pjrt_program(repr(f)))
  outs <- lapply(pjrt_execute(exec), as_array)
  expect_equal(outs[[1]], Inf)
  expect_equal(outs[[2]], -Inf)
  expect_equal(outs[[3]], NaN)
})

test_that("Efficient representation of constants with single value", {
  local_func()
  res <- hlo_tensor(1.0, shape = c(2, 2), dtype = "f32")
  f <- hlo_return(res)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  result_buffer <- pjrt_execute(exec)
  expect_equal(result_buffer, pjrt_buffer(1.0, shape = c(2, 2), dtype = "f32"))
})

test_that("c() shape is interpreted as scalar", {
  local_func()
  x <- hlo_tensor(1L, shape = NULL)
  f <- hlo_return(x)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  buffer <- pjrt_execute(exec)
  expect_equal(buffer, pjrt_scalar(1L))
})
