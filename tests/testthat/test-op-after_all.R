test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  z <- hlo_after_all(x)
  xz <- c(x, z)
  result_func <- hlo_return(x)
  expect_snapshot(repr(result_func))
})
