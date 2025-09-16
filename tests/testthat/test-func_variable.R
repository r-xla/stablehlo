test_that("c works", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2, 2))
  y <- hlo_input("y", "f32", shape = c(2, 2))
  z <- c(x, y)
  expect_list(z, types = "stablehlo::FuncVariable", len = 2L)
  expect_null(names(z))
  expect_snapshot(z[[1]]@func)
  expect_snapshot(z[[2]]@func)

  x2 <- hlo_add(x, y)
  y2 <- hlo_add(x, y)
  outs <- c(x2, y2)
  f <- hlo_return(outs[[1]], outs[[2L]])
  expect_snapshot(f)
})

test_that("repr", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2, 2))
  expect_snapshot(x)
})
