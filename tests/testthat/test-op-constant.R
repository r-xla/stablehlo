test_that("scalars", {
  local_reset_id_gen()
  check <- function(x, elt_type = NULL) {
    f <- if (!is.null(elt_type)) {
      hlo_scalar(x, elt_type)
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
  local_reset_id_gen()
  skip_if_not_installed("pjrt")
  check <- function(x, elt_type) {
    f <- hlo_return(hlo_scalar(x, elt_type))
    f@id = FuncId("main")
    program <- pjrt::pjrt_program(repr(f))
    exec <- pjrt::pjrt_compile(program)
    buffer <- pjrt::pjrt_execute(exec)
    expect_equal(
      pjrt::as_array(buffer),
      x,
      tolerance = if (startsWith(elt_type, "f")) 1e-6 else 0
    )
  }
  check(3.14, "f32")
  check(-8.23, "f32")
  check(3L, "i32")
  check(3L, "i64")
  check(3L, "i16")
  check(3L, "ui32")
  check(3L, "ui64")
  check(3L, "ui16")
  check(TRUE, "pred")
  check(FALSE, "pred")
  skip_if_metal()
  check(-8.23, "f64")
  check(3.14, "f64")
})

test_that("arrays", {
  local_reset_id_gen()
  expect_snapshot(repr(hlo_tensor(array(1:2))@func))
  expect_snapshot(repr(hlo_tensor(array(1:6, dim = c(2, 3)))@func))
  expect_snapshot(repr(hlo_tensor(array(1:6, dim = c(2, 3, 1)))@func))
})

test_that("compile tensors", {
  local_reset_id_gen()
  skip_if_not_installed("pjrt")
  check <- function(x, elt_type) {
    f <- hlo_return(hlo_tensor(x, elt_type))
    f@id = FuncId("main")
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
  local_reset_id_gen()
  expect_error(hlo_scalar(-1L, "ui16"), "must be non-negative")
  expect_error(hlo_scalar(NA), "must not contain NA")
  expect_error(hlo_scalar(3L, "f32"), "Invalid elt_type for integer")
  expect_error(hlo_scalar(1, "i32"), "Invalid elt_type for double")
  expect_error(hlo_scalar(1:2), "a single value")
})

test_that("specify shape in hlo_tensor", {
  local_reset_id_gen()
  expect_snapshot(repr(hlo_tensor(1:2, shape = c(2, 1))@func))
  expect_snapshot(repr(hlo_tensor(1:2)@func))
  expect_snapshot(repr(hlo_tensor(1)@func))
})
