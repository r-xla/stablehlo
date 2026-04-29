test_that("basic tests", {
  func_values <- local_func()
  xv <- hlo_input("x", "f32", shape = c(2L, 5L))
  resv <- hlo_top_k(xv, k = 3L)
  fv <- hlo_return(resv[[1L]], func = func_values)

  func_indices <- local_func()
  xi <- hlo_input("x", "f32", shape = c(2L, 5L))
  resi <- hlo_top_k(xi, k = 3L)
  fi <- hlo_return(resi[[2L]], func = func_indices)

  expect_snapshot(repr(fv))
  expect_snapshot(repr(fi))

  skip_if_not_installed("pjrt")
  exec_v <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(fv)))
  exec_i <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(fi)))

  data <- matrix(c(5, 1, 3, 2, 4, 9, 7, 8, 6, 10), nrow = 2L, byrow = TRUE)
  buf <- pjrt::pjrt_buffer(data, dtype = "f32")

  out_v <- pjrt::pjrt_execute(exec_v, buf)
  expect_equal(
    pjrt::as_array(out_v),
    matrix(c(5, 4, 3, 10, 9, 8), nrow = 2L, byrow = TRUE)
  )

  out_i <- pjrt::pjrt_execute(exec_i, buf)
  expect_equal(
    pjrt::as_array(out_i),
    matrix(c(0L, 4L, 2L, 4L, 0L, 2L), nrow = 2L, byrow = TRUE)
  )
})

test_that("output types and shapes", {
  # 1-D input
  vt_out <- infer_types_top_k(vt("f32", 8L), k = scnst(3L, "i64"))
  expect_length(vt_out, 2L)
  expect_equal(shape(vt_out[[1L]]), 3L)
  expect_equal(shape(vt_out[[2L]]), 3L)
  expect_equal(vt_out[[1L]]$type$dtype, FloatType(32L))
  expect_equal(vt_out[[2L]]$type$dtype, IntegerType(32L))

  # higher-rank input — only last dim changes
  vt_out <- infer_types_top_k(vt("i64", c(2L, 3L, 7L)), k = scnst(2L, "i64"))
  expect_equal(shape(vt_out[[1L]]), c(2L, 3L, 2L))
  expect_equal(shape(vt_out[[2L]]), c(2L, 3L, 2L))
  expect_equal(vt_out[[1L]]$type$dtype, IntegerType(64L))
  expect_equal(vt_out[[2L]]$type$dtype, IntegerType(32L))

  # k = 0 is allowed
  vt_out <- infer_types_top_k(vt("f64", 5L), k = scnst(0L, "i64"))
  expect_equal(shape(vt_out[[1L]]), 0L)
})

test_that("errors", {
  # rank 0 operand
  expect_snapshot(
    infer_types_top_k(vt("f32", integer()), k = scnst(1L, "i64")),
    error = TRUE
  )

  # k > last dim
  expect_snapshot(
    infer_types_top_k(vt("f32", c(2L, 3L)), k = scnst(5L, "i64")),
    error = TRUE
  )

  # negative k
  expect_snapshot(
    infer_types_top_k(vt("f32", c(2L, 3L)), k = scnst(-1L, "i64")),
    error = TRUE
  )

  # unsupported dtype (boolean)
  expect_snapshot(
    infer_types_top_k(vt("pred", c(2L, 3L)), k = scnst(1L, "i64")),
    error = TRUE
  )
})
