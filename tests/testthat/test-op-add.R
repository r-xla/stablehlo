test_that("basic tests", {
  hlo_test_biv(hlo_add, `+`, tol = 1e-6)
})
