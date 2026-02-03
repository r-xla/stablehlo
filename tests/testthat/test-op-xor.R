test_that("XOR works", {
  hlo_test_biv(
    hlo_xor,
    function(lhs, rhs) {
      (lhs | rhs) & !(lhs & rhs)
    },
    dtypes = "pred"
  )
})

# Errors are tested in test-type_inference.R (via infer_types_integerish_biv)
