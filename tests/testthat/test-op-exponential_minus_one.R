test_that("basic tests", {
  hlo_test_uni(hlo_exponential_minus_one, function(x) exp(x) - 1, tol = 1e-6)
})
