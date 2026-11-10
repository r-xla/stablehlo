test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  y <- hlo_input("y", "i32", shape = integer())
  out <- hlo_optimization_barrier(x, y)
  f <- hlo_return(out[[1L]], out[[2L]])
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  executable <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(f)))
  x_buf <- pjrt::pjrt_buffer(matrix(1:6 / 2, 2L, 3L), dtype = "f32")
  y_buf <- pjrt::pjrt_scalar(7L, dtype = "i32")
  res <- pjrt::pjrt_execute(executable, x_buf, y_buf)
  expect_equal(res[[1L]], x_buf)
  expect_equal(res[[2L]], y_buf)
})

test_that("each result has the type of its operand", {
  # (C1) `type(operand...) = type(result...)`.
  local_func()
  x <- hlo_input("x", "f64", shape = 4L)
  token <- hlo_after_all()
  out <- hlo_optimization_barrier(x, token)
  expect_equal(out[[1L]]$value_type, x$value_type)
  expect_equal(out[[2L]]$value_type, token$value_type)
})

test_that("a single operand gives a single value unless simplify = FALSE", {
  local_func()
  x <- hlo_input("x", "f32", shape = 2L)
  expect_s3_class(hlo_optimization_barrier(x), "FuncValue")
  out <- hlo_optimization_barrier(x, simplify = FALSE)
  expect_length(out, 1L)
  expect_s3_class(out[[1L]], "FuncValue")
})
