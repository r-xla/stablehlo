test_that("add works", {
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  y <- hlo_input("y", "f32", shape = c(2L, 2L), "main")
  z <- hlo_add(x, y)
  func <- hlo_return(y)
  expect_snapshot(repr(func))
})
