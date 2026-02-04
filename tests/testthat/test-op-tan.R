test_that("basic tests", {
  hlo_test_uni(hlo_tan, tan)
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
