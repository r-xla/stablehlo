test_that("basic tests", {
  hlo_test_uni(hlo_is_finite, is.finite)
})

# Errors are tested in test-assert.R (via assert_vt_has_ttype)

test_that("is_finite renders in the generic form even when told its types match", {
  # The ODS overrides `is_finite`'s assembly with
  # `functional-type(operands, results)`, so the short form does not parse.
  # Inference always gives it a `pred` result, but `output_types` skips
  # inference, and that is the path a lowering uses.
  local_func()
  x <- hlo_input("x", "f32", shape = 3L)
  f <- hlo_return(hlo_is_finite(
    x,
    output_types = list(ValueType("f32", shape = 3L))
  ))
  expect_match(repr(f), '"stablehlo.is_finite"', fixed = TRUE)
})

test_that("an elementwise op keeps the short assembly form", {
  # The flag is opt-out: only an op whose ODS overrides `assemblyFormat` loses
  # the short form, so the ops that have it must keep it.
  local_func()
  x <- hlo_input("x", "f32", shape = 3L)
  f <- hlo_return(hlo_negate(x))
  expect_match(repr(f), "= stablehlo.negate ", fixed = TRUE)
})
