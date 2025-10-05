test_that("basic tests", {
  hlo_test_uni(
    hlo_popcnt,
    \(x) {
      result <- vapply(x, \(y) sum(intToBits(y) == TRUE), integer(1))
      array(result, dim = dim(x))
    },
    dtype = "i32"
  )
})
