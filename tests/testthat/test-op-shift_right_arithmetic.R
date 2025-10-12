test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_right_arithmetic,
    \(x, n) {
      x_ <- c(x)
      n_ <- c(n)
      M <- data.frame(x_, n_)
      M$bits <- lapply(x_, \(y) rev(as.integer(intToBits(y))))
      inner_shift <- function(x, n) {
        l <- length(x)
        temp <- integer(l)
        if (n >= l) {
          temp <- rep(x[[1]], l)
        } else {
          temp <- c(rep(x[[1]], n), x[1:(l - n)])
        }

        if (temp[[1]]) {
          temp <- as.integer(!temp)
          return(-sum(2^rev(seq_len(l) - 1) * temp) - 1)
        } else {
          return(sum(2^rev(seq_len(l) - 1) * temp))
        }
      }
      array(
        apply(M, MARGIN = 1, FUN = \(z) inner_shift(z[[3]], z[[2]])),
        dim = dim(x)
      )
    },
    non_negative = list(FALSE, TRUE),
    dtypes = "i32"
  )
})
