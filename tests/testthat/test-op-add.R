test_that("basic tests", {
  hlo_test_biv(hlo_add, `+`)
})

# error tests for infer_types_generic_biv are in test-type_inference.R
