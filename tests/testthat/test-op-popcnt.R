test_that("basic tests", {
  hlo_test_uni(
    hlo_popcnt,
    \(x) {
      result <- vapply(x, \(y) sum(as.logical(intToBits(y))), integer(1))
      array(result, dim = dim(x))
    },
    dtype = "i32"
  )
})

# Errors are tested in test-type_inference.R (via infer_types_integer_uni)
