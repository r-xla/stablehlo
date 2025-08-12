test_that("and works", {
  local_reset_id_gen()
  x <- hlo_input("x", "pred", shape = integer(), "main")
  y <- hlo_input("y", "pred", shape = integer(), "main")
  z <- hlo_and(x, y)
  func <- hlo_return(z)
  expect_snapshot(repr(func))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  exec <- pjrt_compile(program)

  expect_equal(
    pjrt_execute(exec, pjrt_scalar(TRUE), pjrt_scalar(TRUE)),
    pjrt_scalar(TRUE)
  )
  expect_equal(
    pjrt_execute(exec, pjrt_scalar(TRUE), pjrt_scalar(FALSE)),
    pjrt_scalar(FALSE)
  )
})
