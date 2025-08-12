test_that("consistent with formatC", {
  check <- function(x, precision) {
    digits <- if (precision == 32) 8 else 16
    expect_equal(
      format_double(x, precision),
      formatC(x, format = "e", digits = digits)
    )
  }
  check(1.23, 32)
  check(1.23, 64)
  check(1.234, 32)
  check(1.234, 64)
  check(-1.234, 64)
  check(-1022.234, 64)
  expect_equal(format_double(double()), character())
  expect_error(format_double(1L), "double")
})
