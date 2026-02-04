test_that("basic tests", {
  hlo_test_uni(hlo_logistic, \(x) 1 / (1 + exp(-x)))
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
