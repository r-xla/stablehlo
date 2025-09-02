test_that("basic tests",
          {hlo_test_uni(hlo_sqrt,
                        sqrt,
                        non_negative = TRUE,
                        tol = 1e-6)})
