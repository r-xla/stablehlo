test_that("return cleans up afterwards", {
  f <- local_func("f")
  g <- (function() {
    x <- hlo_input("x", "f32", shape = c(2, 2))
    hlo_return(x)
  })()
  expect_equal(.current_func(), f)
})
