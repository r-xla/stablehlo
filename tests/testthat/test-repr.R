test_that("same constant gets same id", {
  x <- hlo_scalar(1L, "i32")
  y <- hlo_scalar(1L, "i32")
  f <- hlo_return(x, x, y)
  expect_snapshot(repr(f))
})
