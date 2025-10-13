test_that("basic tests", {
  hlo_test_uni(hlo_log_plus_one, \(x) log(x + 1), non_negative = TRUE)
})
