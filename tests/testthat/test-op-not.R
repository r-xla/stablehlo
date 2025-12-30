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

test_that("error", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  expect_snapshot(hlo_not(x), error = TRUE)
})
