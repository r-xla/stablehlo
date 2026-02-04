test_that("basic tests", {
  hlo_test_uni(hlo_log_plus_one, \(x) log(x + 1), non_negative = TRUE)
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
