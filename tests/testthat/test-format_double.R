test_that("format_double emits 32-bit hex bit-patterns for non-finite values", {
  expect_equal(format_double(NaN, 32), "0x7FC00000")
  expect_equal(format_double(Inf, 32), "0x7F800000")
  expect_equal(format_double(-Inf, 32), "0xFF800000")
})

test_that("format_double emits width-correct 64-bit hex bit-patterns for non-finite values", {
  # Previously these wrongly reused the 32-bit patterns, so an f64 NaN/Inf
  # constant was parsed as a tiny denormal (~1e-314) instead of NaN/Inf.
  expect_equal(format_double(NaN, 64), "0x7FF8000000000000")
  expect_equal(format_double(Inf, 64), "0x7FF0000000000000")
  expect_equal(format_double(-Inf, 64), "0xFFF0000000000000")
})

test_that("format_double keeps non-finite positions in a mixed vector", {
  out32 <- format_double(c(1, NaN, Inf, -Inf), 32)
  expect_equal(out32[2:4], c("0x7FC00000", "0x7F800000", "0xFF800000"))
  expect_match(out32[1], "^1")

  out64 <- format_double(c(1, NaN, Inf, -Inf), 64)
  expect_equal(out64[2:4], c("0x7FF8000000000000", "0x7FF0000000000000", "0xFFF0000000000000"))
  expect_match(out64[1], "^1")
})
