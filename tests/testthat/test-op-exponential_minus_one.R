test_that("basic tests", {
  hlo_test_uni(hlo_exponential_minus_one, \(x) exp(x) - 1)
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
