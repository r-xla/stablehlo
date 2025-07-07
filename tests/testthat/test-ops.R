test_that("hlo_constant works with numeric values", {
  # Test with a positive value
  op <- hlo_constant(3.14)
  expect_snapshot(repr(op))
})
