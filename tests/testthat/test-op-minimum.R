test_that("basic tests", {
  hlo_test_biv(hlo_minimum, pmin, tol = 1e-6)
})

# Errors are tested in test-type_inference.R (via infer_types_generic_biv)
