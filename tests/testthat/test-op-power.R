test_that("basic tests",
          {hlo_test_biv(hlo_power, function(lhs, rhs) lhs^rhs, tol = 1e-6)})
