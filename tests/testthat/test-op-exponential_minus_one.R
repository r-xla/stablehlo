test_that("basic tests", {
  hlo_test_uni(hlo_exponential_minus_one, \(x) exp(x) - 1)
})
