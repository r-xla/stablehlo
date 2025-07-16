test_that("If operator works", {
  local_reset_id_gen()
  x1 <- hlo_input("x1", "f32", shape = integer())
  x2 <- hlo_input("x2", "f32", shape = integer())

  f1 <- hlo_return(hlo_add(x1, x1))
  f2 <- hlo_return(hlo_abs(x2))

  which <- hlo_input("x", "i1", integer(), func_id = "main")
  out <- hlo_if(
    pred = which,
    true_branch = f1,
    false_branch = f2
  )
  f <- hlo_return(out)
  expect_snapshot(f)

  skip_if_not_installed("pjrt")
  # TODO: Make this work
  #program <- pjrt_program(repr(f))
  #expect_class(program, "PJRTProgram")

  #executable <- pjrt_compile(program)
  #expect_class(executable, "PJRTLoadedExecutable")

  #out <- pjrt_execute(executable, pjrt_scalar(TRUE), pjrt_scalar(1))
})
