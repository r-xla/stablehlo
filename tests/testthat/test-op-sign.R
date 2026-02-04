test_that("basic tests", {
  hlo_test_uni(hlo_sign, sign)
})

# Errors are tested in test-assert.R (via assert_vt_is_tensor, assert_vt_has_ttype)
