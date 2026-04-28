test_that("basic tests", {
  hlo_test_uni(
    hlo_is_pos_inf,
    function(x) is.infinite(x) & x > 0,
    dimension = c(3L, 3L),
    test_data = c(
      Inf,
      -Inf,
      NaN,
      0,
      1,
      -1,
      1e30,
      -1e30,
      0.5
    )
  )
})

# Errors are tested in test-assert.R (via assert_vt_has_ttype)
