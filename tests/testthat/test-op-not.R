test_that("basic tests", {
  hlo_test_uni(
    hlo_not,
    \(x) {
      if (is.logical(x)) {
        return(!x)
      } else {
        if (length(dim(x)) > 1) {
          array(bitwNot(as.integer(x)), dim(x))
        } else {
          array(bitwNot(as.integer(x)), length(x))
        }
      }
    },
    dtype = c("pred", "i32")
  )
})

# Errors are tested in test-type_inference.R (via infer_types_integerish_uni)
