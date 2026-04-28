test_that("basic tests", {
  hlo_test_biv(
    hlo_polygamma,
    function(n, x) psigamma(x, deriv = n[[1L]]),
    dimension = c(3L, 3L),
    lhs = rep(1, 9), # n = 1 (trigamma) for all elements
    rhs = seq(0.5, 5, length.out = 9)
  )
})

# Errors are tested in test-type_inference.R (via infer_types_float_biv)
