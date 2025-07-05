test_that("stablehlo_constant works with numeric values", {
  # Test with a positive value
  op <- stablehlo_constant(3.14)
  expect_snapshot(repr(op))
})
