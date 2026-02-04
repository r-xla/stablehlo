test_that("errors", {
  pred <- vt("pred", integer())
  branch_i32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("i32", 2L))))
  )
  branch_f32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("f32", 2L))))
  )
  branch_2out <- Func(
    outputs = FuncOutputs(list(
      FuncOutput(vt("i32", 2L)),
      FuncOutput(vt("i32", 3L))
    ))
  )
  # different number of outputs
  expect_snapshot(
    infer_types_if(pred, branch_i32, branch_2out),
    error = TRUE
  )
  # output types don't match
  expect_snapshot(
    infer_types_if(pred, branch_i32, branch_f32),
    error = TRUE
  )
})

test_that("If operator works", {
  func <- local_func()
  pred <- hlo_input("pred", "i1", integer())
  x1 <- hlo_input("x1", "f32", integer())
  x2 <- hlo_input("x2", "f32", integer())

  f1 <- hlo_return(hlo_closure(x1)[[1L]])
  f2 <- hlo_return(hlo_closure(x2)[[1L]])

  out <- hlo_if(
    pred = pred,
    true_branch = f1,
    false_branch = f2
  )
  f <- hlo_return(out)
  expect_snapshot(f)

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  expect_equal(
    as_array(pjrt_execute(
      executable,
      pjrt_scalar(TRUE),
      pjrt_scalar(1),
      pjrt_scalar(2)
    )),
    as_array(pjrt_scalar(1))
  )
  expect_equal(
    as_array(pjrt_execute(
      executable,
      pjrt_scalar(FALSE),
      pjrt_scalar(1),
      pjrt_scalar(2)
    )),
    as_array(pjrt_scalar(2))
  )
})
