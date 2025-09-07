test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_left,
    \(x, y) {
      if (length(dim(x)) > 1) {
        res <- array(bitwShiftL(as.integer(x), as.integer(y)), dim(x))
      } else {
        res <- array(bitwShiftL(as.integer(x), as.integer(y)), length(x))
      }
      res[is.na(res)] <- 0
      return(res)
    },
    dtype = "i32"
  )
})
