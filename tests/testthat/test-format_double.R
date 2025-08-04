test_that("NaN", {
  expect_equal(format_double(NaN), "nan")
})
