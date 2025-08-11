test_that("atan2 works", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = integer(), "main")
  y <- hlo_input("y", "f32", shape = integer(), "main")
  z <- hlo_atan2(x, y)
  func <- hlo_return(z)
  expect_snapshot(repr(func))
  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  exec <- pjrt_compile(program)
  expect_equal(
    pjrt_execute(exec, pjrt_scalar(1), pjrt_scalar(0)),
    pjrt_scalar(pi / 2)
  )
})
