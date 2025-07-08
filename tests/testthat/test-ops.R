test_that("op_constant works with numeric values", {
  # Test with a positive value
  op <- op_constant(3.14)
  expect_snapshot(repr(op))
})
