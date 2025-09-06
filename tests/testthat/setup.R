old_opts <- options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

# https://github.com/HenrikBengtsson/Wishlist-for-R/issues/88
old_opts <- lapply(old_opts, function(x) if (is.null(x)) FALSE else x)

library("testthat")
library("checkmate")

if (requireNamespace("pjrt", quietly = TRUE)) {
  library("pjrt")
}
