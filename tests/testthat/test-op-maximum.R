test_that("basic tests", {
  hlo_test_biv(hlo_maximum, pmax)
})

# Errors are tested in test-type_inference.R (via infer_types_generic_biv)
