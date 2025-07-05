test_that("builder works", {
  x <- stablehlo_input("x", "f32", shape = c(2L, 2L), "main")
  y <- stablehlo_input("y", "f32", shape = c(2L, 2L))
  z <- stablehlo_add(x, y)
  func <- stablehlo_return(z)
  expect_snapshot(repr(func))
})
