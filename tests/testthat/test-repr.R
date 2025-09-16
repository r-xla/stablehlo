test_that("same constant gets same id", {
  func <- local_func()
  x <- hlo_scalar(1L, dtype = "i32")
  y <- hlo_scalar(1L, dtype = "i32")
  f <- hlo_return(x, x, y)
  expect_snapshot(repr(f))
})
