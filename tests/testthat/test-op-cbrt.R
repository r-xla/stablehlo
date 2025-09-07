test_that("basic tests", {
  hlo_test_uni(hlo_cbrt, \(x) x^(1 / 3), non_negative = TRUE)
})
