test_that("XOR works", {
  hlo_test_biv(
    hlo_xor,
    function(lhs, rhs) {
      (lhs | rhs) & !(lhs & rhs)
    },
    type = "pred"
  )
})
