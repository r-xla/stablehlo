test_that("basic tests", {
  hlo_test_uni(
    hlo_bessel_i1e,
    function(x) besselI(x, nu = 1, expon.scaled = TRUE),
    dimension = c(3L, 3L),
    test_data = seq(0.1, 5, length.out = 9)
  )
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
