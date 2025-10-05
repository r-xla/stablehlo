test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_right_arithmetic,
    \(x, n) {
      result <- sign(x) * array(bitwShiftR(abs(x), n), dim(x))
      result[result == 0 & sign(x) == -1] <- -1
      result
    },
    non_negative = list(FALSE, TRUE),
    dtype = "i32"
  )
})
