test_that("basic tests", {
  hlo_test_uni(hlo_abs, abs, tol = 1e-5)
})

# error tests for infer_types_numeric_uni are in test-type_inference.R
