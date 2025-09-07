test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_right_arithmetic,
    \(x, y) {
      if (length(dim(x)) > 1) {
        res <- array(bitwShiftR(as.integer(x), as.integer(y)), dim(x))
      } else {
        res <- array(bitwShiftR(as.integer(x), as.integer(y)), length(x))
      }
      res[is.na(res)] <- 0
      return(res)
    },
    non_negative = TRUE,
    dtype = "i32"
  )
})
