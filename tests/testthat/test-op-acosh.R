test_that("basic tests", {
  hlo_test_uni(
    hlo_acosh,
    acosh,
    dimension = c(3L, 3L),
    test_data = seq(1.1, 5, length.out = 9)
  )
})

# Errors are tested in test-type_inference.R (via infer_types_float_uni)
