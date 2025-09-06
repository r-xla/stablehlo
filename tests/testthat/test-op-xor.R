test_that("basic tests", {
  hlo_test_biv(
    hlo_xor,
    function(lhs, rhs) {
      (lhs | rhs) & !(lhs & rhs)
    },
    dtype = "pred"
  )
})
