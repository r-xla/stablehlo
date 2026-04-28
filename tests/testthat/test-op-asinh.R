test_that("basic tests", {
  hlo_test_uni(hlo_asinh, asinh)
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
