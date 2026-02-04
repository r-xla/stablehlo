test_that("basic tests", {
  hlo_test_biv(hlo_or, `|`, dtypes = "pred")
})

# Errors are tested in test-type_inference.R (via infer_types_integerish_biv)
