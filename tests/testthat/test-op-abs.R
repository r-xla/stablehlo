test_that("basic tests", {
  hlo_test_uni(hlo_abs, abs, tol = 1e-5)
})

# error tests for infer_types_numeric_uni are in test-type_inference.R

test_that("abs rejects unsigned operands", {
  # (I1) is "signed integer, floating-point, ..." -- `HLO_SInt` in the ODS, so
  # unsigned is out. It used to be accepted and only MLIR refused it.
  local_func()
  expect_error(hlo_abs(hlo_input("a", "ui32", shape = 3L)), "must have dtype")
  expect_equal(
    inferred(function() hlo_abs(dyn_input("a", "i32", 3L))),
    "tensor<3xi32>"
  )
})
