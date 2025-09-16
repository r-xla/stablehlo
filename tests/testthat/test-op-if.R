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
