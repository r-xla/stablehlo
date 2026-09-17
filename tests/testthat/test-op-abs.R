test_that("basic tests", {
  hlo_test_uni(hlo_abs, abs, tol = 1e-5)
})

# error tests for infer_types_numeric_uni are in test-type_inference.R

test_that("unsigned operands are rejected", {
  # (I1) is "signed integer, floating-point, ..." -- `HLO_SInt` in the ODS, so
  # unsigned is out. `hlo_abs()` used the shared `infer_types_numeric_uni()`,
  # which admits `uint`, leaving its own `infer_types_abs()` unused.
  expect_snapshot(infer_types_abs(vt("ui32", 3L)), error = TRUE)
  expect_equal(repr(infer_types_abs(vt("i32", 3L))[[1L]]$type), "tensor<3xi32>")
})
