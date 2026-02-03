test_that("basic tests", {
  hlo_test_biv(hlo_atan2, atan2)
})

# error tests for infer_types_float_biv are in test-type_inference.R
