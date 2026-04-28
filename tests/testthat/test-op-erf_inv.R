test_that("basic tests", {
  hlo_test_uni(
    hlo_erf_inv,
    function(x) qnorm((x + 1) / 2) / sqrt(2),
    dimension = c(3L, 3L),
    test_data = seq(-0.9, 0.9, length.out = 9)
  )
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
