test_that("Case operator works", {
  func <- local_func()
  index <- hlo_input("index", "i32", integer())
  x1 <- hlo_input("x1", "i64", 2L)
  x2 <- hlo_input("x2", "i64", 2L)

  c0 <- hlo_closure(x1)
  f_b0 <- hlo_return(c0[[1L]])
  c1 <- hlo_closure(x2)
  f_b1 <- hlo_return(c1[[1L]])

  out <- hlo_case(index, f_b0, f_b1)

  f <- hlo_return(out)
  expect_snapshot(f)

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  executable <- pjrt_compile(program)

  # last branch is picked if index is out of range
  out <- pjrt_execute(
    executable,
    pjrt_scalar(-1L),
    pjrt_buffer(c(0L, 0L), dtype = "i64"),
    pjrt_buffer(c(1L, 1L), dtype = "i64")
  )
  expect_equal(as_array(out), array(c(1L, 1L), dim = 2L))

  out <- pjrt_execute(
    executable,
    pjrt_scalar(0L),
    pjrt_buffer(c(0L, 0L), dtype = "i64"),
    pjrt_buffer(c(1L, 1L), dtype = "i64")
  )
  expect_equal(as_array(out), array(c(0L, 0L), dim = 2L))

  out <- pjrt_execute(
    executable,
    pjrt_scalar(1L),
    pjrt_buffer(c(0L, 0L), dtype = "i64"),
    pjrt_buffer(c(1L, 1L), dtype = "i64")
  )
  expect_equal(as_array(out), array(c(1L, 1L), dim = 2L))
})

test_that("errors", {
  index <- vt("i32", integer())
  branch_i32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("i32", c(2L)))))
  )
  branch_f32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("f32", c(2L)))))
  )
  branch_with_input <- Func(
    inputs = FuncInputs(list(FuncInput(ValueId("x"), vt("i32", c(2L))))),
    outputs = FuncOutputs(list(FuncOutput(vt("i32", c(2L)))))
  )

  # branches must be non-empty
  expect_snapshot(infer_types_case(index), error = TRUE)
  # branches must be Func objects
  expect_snapshot(infer_types_case(index, "not a func"), error = TRUE)
  # branch functions must not have inputs
  expect_snapshot(infer_types_case(index, branch_with_input), error = TRUE)
  # all branch functions must have the same output types
  expect_snapshot(
    infer_types_case(index, branch_i32, branch_f32),
    error = TRUE
  )
})
