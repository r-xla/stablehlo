test_that("basic tests", {
  hlo_test_uni(hlo_cbrt, \(x) x^(1 / 3), non_negative = TRUE)
})

# error tests for infer_types_float_uni are in test-type_inference.R
