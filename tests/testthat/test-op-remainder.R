test_that("basic tests", {
  hlo_test_biv(
    hlo_remainder,
    function(lhs, rhs) {
      neg <- lhs < 0
      res <- (abs(lhs) %% abs(rhs))
      res[neg] <- res[neg] * -1
      res
    },
    tol = 1e-6
  )
})
