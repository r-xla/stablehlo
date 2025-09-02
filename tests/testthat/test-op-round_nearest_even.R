test_that("basic tests", {
  hlo_test_uni(hlo_round_nearest_even, round, tol = 1e-6)
})
