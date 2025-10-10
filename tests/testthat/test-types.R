test_that("FloatType repr", {
  ft <- FloatType(32L)
  expect_equal(repr(ft), "f32")
})

test_that("Boolean Type repr", {
  bt <- BooleanType()
  expect_equal(repr(bt), "i1")
})

test_that("TensorType repr", {
  tt <- TensorType(
    dtype = FloatType(32L),
    shape = Shape(c(1L, 2L))
  )
  expect_equal(repr(tt), "tensor<1x2xf32>")
})

test_that("as_dtype", {
  expect_equal(as_dtype("f32"), FloatType(32L))
  expect_equal(as_dtype(BooleanType()), BooleanType())
})

test_that("is_dtype", {
  expect_true(is_dtype(BooleanType()))
  expect_false(is_dtype("i32"))
})
