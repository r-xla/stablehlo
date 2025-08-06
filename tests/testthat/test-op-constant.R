test_that("scalars", {
  expect_snapshot(repr(op_constant(3.14)))
  expect_snapshot(repr(op_constant(3.14, "f32")))
  expect_snapshot(repr(op_constant(3.14, "f64")))
  expect_snapshot(repr(op_constant(3L)))
  expect_snapshot(repr(op_constant(3L, "i32")))
  expect_snapshot(repr(op_constant(3L, "i64")))
  expect_snapshot(repr(op_constant(3L, "i16")))
  expect_snapshot(repr(op_constant(3L, "ui32")))
  expect_snapshot(repr(op_constant(3L, "ui64")))
  expect_snapshot(repr(op_constant(3L, "ui16")))
  expect_snapshot(repr(op_constant(-3L)))
  expect_snapshot(repr(op_constant(-100L)))
  expect_snapshot(repr(op_constant(TRUE)))
  expect_snapshot(repr(op_constant(FALSE)))
})

test_that("compile scalars", {
  skip_if_not_installed("pjrt")
  check <- function(x, dtype) {
    f <- hlo_return(hlo_scalar(x, dtype))
    f@id = FuncId("main")
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
  check(3.14, "f64")
  check(-8.23, "f32")
  check(-8.23, "f64")
  check(3L, "i32")
  check(3L, "i64")
  check(3L, "i16")
  check(3L, "ui32")
  check(3L, "ui64")
  check(3L, "ui16")
  check(TRUE, "pred")
  check(FALSE, "pred")
})

test_that("arrays", {
  expect_snapshot(repr(op_constant(array(1:2))))
  expect_snapshot(repr(op_constant(array(1:6, dim = c(2, 3)))))
  expect_snapshot(repr(op_constant(array(1:6, dim = c(2, 3, 1)))))
})

test_that("compile tensors", {
  skip_if_not_installed("pjrt")
  check <- function(x, dtype) {
    f <- hlo_return(hlo_tensor(x, dtype))
    f@id = FuncId("main")
    program <- pjrt::pjrt_program(repr(f))
    exec <- pjrt::pjrt_compile(program)
    buffer <- pjrt::pjrt_execute(exec)
    expect_equal(
      pjrt::as_array(buffer),
      x
    )
  }
  #check(array(1:2), "i32")
  #check(array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3)), "f32")
  #check(array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3, 1)), "f32")
})


test_that("errors", {
  expect_error(hlo_scalar(-1L, "ui16"), "must be non-negative")
  expect_error(hlo_scalar(NA), "must not contain NA")
  expect_error(hlo_scalar(3L, "f32"), "Invalid elt_type for integer")
  expect_error(hlo_scalar(1, "i32"), "Invalid elt_type for double")
  expect_error(hlo_scalar(1:2), "a single atomic value")
})
