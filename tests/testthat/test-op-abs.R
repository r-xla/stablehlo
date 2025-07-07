test_that("abs works", {
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  y <- hlo_abs(x)
  func <- hlo_return(y)
  expect_snapshot(repr(func))
})
