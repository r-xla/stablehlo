test_that("basic tests", {
  hlo_test_uni(
    hlo_rsqrt,
    function(x) 1 / sqrt(x),
    non_negative = TRUE,
    tol = 1e-6
  )
})
