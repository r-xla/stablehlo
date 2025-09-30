test_that("FloatType repr", {
  ft <- FloatType("f32")
  expect_equal(repr(ft), "f32")
})

test_that("Boolean Type repr", {
  bt <- BooleanType()
  expect_equal(repr(bt), "i1")
})

test_that("TensorType repr", {
  tt <- TensorType(
    dtype = FloatType("f32"),
    shape = Shape(c(1L, 2L))
  )
  expect_equal(repr(tt), "tensor<1x2xf32>")
})

test_that("as_dtype", {
  expect_equal(as_dtype("f32"), FloatType("f32"))
  expect_equal(as_dtype(BooleanType()), BooleanType())
})
