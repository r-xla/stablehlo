test_that("op_constant works with numeric values", {
  # Test with a positive value
  op <- op_constant(3.14)
  expect_snapshot(repr(op))
})

test_that("Can create a function with no inputs", {
  local_reset_id_gen()
  x <- hlo_constant(3.14)
  f <- hlo_return(x)
  expect_snapshot(repr(f))
})
