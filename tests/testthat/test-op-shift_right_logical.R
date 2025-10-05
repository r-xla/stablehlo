test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_right_logical,
    \(x, n) {
      result <- array(bitwShiftR(x, n), dim(x))
      result[is.na(result)] <- 0
      result
    },
    non_negative = list(FALSE, TRUE),
    dtype = "i32"
  )
})
