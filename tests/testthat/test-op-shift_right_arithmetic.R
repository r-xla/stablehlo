test_that("basic tests", {
  hlo_test_biv(
    hlo_shift_right_arithmetic,
    \(x, n) {
      M <- data.frame(c(x), c(n))
      inner_shift <- function(x, n) {
        x_bits <- rev(as.integer(intToBits(x)))
        l <- length(x_bits)
        temp <- integer(l)
        if (n >= l) {
          temp <- rep(x_bits[[1]], l)
        } else {
          temp <- c(rep(x_bits[[1]], n), x_bits[1:(l - n)])
        }
        if (temp[[1]]) {
          temp <- as.integer(!temp)
          return(-sum(2^rev(seq_len(l) - 1) * temp) - 1)
        } else {
          return(sum(2^rev(seq_len(l) - 1) * temp))
        }
      }
      array(
        apply(M, MARGIN = 1, FUN = \(z) inner_shift(z[[1]], z[[2]])),
        dim = dim(x)
      )
    },
    non_negative = list(FALSE, TRUE),
    dtypes = "i32"
  )
})

# Errors are tested in test-type_inference.R (via infer_types_integerish_biv)
