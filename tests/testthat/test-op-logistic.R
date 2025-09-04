test_that("basic tests", {
  hlo_test_uni(hlo_logistic, function(x) 1 / (1 + exp(-x)), tol = 1e-6)
})
