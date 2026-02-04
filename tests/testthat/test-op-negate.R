test_that("basic tests", {
  hlo_test_uni(hlo_negate, function(x) -x)
})

# Errors are tested in test-type_inference.R (via infer_types_numeric_uni)
