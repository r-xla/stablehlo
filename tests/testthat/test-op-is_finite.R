test_that("basic tests", {
  hlo_test_uni(hlo_is_finite, is.finite)
})

# Errors are tested in test-assert.R (via assert_vt_has_ttype)

test_that("is_finite renders in the generic form even when told its types match", {
  local_func()
  x <- hlo_input("x", "f32", shape = 3L)
  f <- hlo_return(hlo_is_finite(
    x,
    output_types = list(ValueType("f32", shape = 3L))
  ))
  expect_match(repr(f), '"stablehlo.is_finite"', fixed = TRUE)
})
