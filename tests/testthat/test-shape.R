test_that("shape", {
  shape <- Shape(c(1, 2, NA))
  expect_equal(repr(shape), "1x2x?")
  shape <- Shape(c(1, 2))
  expect_equal(shape(shape), c(1, 2))
  expect_integer(shape(shape))
})
