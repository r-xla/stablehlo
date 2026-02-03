test_that("basic tests", {
  hlo_test_biv(hlo_and, `&`, dtypes = "pred")
})

# error tests for infer_types_integerish_biv are in test-type_inference.R
