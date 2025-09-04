test_that("basic tests", {
  hlo_test_uni(
    hlo_log_plus_one,
    function(x) log(x + 1),
    tol = 1e-6,
    non_negative = TRUE
  )
})
