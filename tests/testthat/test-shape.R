test_that("Shape Repr", {
  shape <- Shape(c(1, 2, NA))
  expect_equal(repr(shape), "1x2x?")
})
