test_that("Shape Repr", {
  shape <- Shape(list(
    DimensionSize(1L),
    DimensionSize(2L),
    DimensionSize("?")
  ))

  expect_equal(repr(shape), "1x2x?x")
})
