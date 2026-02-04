test_that("basic tests", {
  hlo_test_uni(hlo_round_nearest_even, round)
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
