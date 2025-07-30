test_that("and works", {
  local_reset_id_gen()
  x <- hlo_input("x", "i32", shape = c(2L, 2L), "main")
  y <- hlo_input("y", "i32", shape = c(2L, 2L), "main")
  z <- hlo_and(x, y)
  func <- hlo_return(z)
  expect_snapshot(repr(func))
})
