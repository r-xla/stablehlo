test_that("basic tests", {
  hlo_test_biv(
    hlo_remainder,
    function(lhs, rhs) {
      neg <- lhs < 0
      res <- (abs(lhs) %% abs(rhs))
      res[neg] <- res[neg] * -1
      res
    }
  )
})

# Errors are tested in test-type_inference.R (via infer_types_numeric_biv)
