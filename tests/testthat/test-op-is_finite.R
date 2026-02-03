test_that("basic tests", {
  hlo_test_uni(hlo_is_finite, is.finite)
})

# Errors are tested in test-assert.R (via assert_vt_has_ttype)
