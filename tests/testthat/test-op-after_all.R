test_that("after_all works", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  z <- hlo_after_all(x)
  xz <- c(x, z)
  func <- hlo_return(x)
  expect_snapshot(repr(func))
})
