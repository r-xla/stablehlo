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
    elt_type = TensorElementType(type = FloatType("f32")),
    shape = Shape(c(1L, 2L))
  )

  expect_equal(repr(tt), "tensor<1x2xf32>")
})

test_that("ValueType", {
  vt <- ValueType(TensorType(
    elt_type = TensorElementType(type = FloatType("f32")),
    shape = Shape(c(1L, 2L))
  ))
  vt
  expect_equal(repr(vt), "tensor<1x2xf32>")
})

library(S7)
A = new_class(
  "A",
  constructor = function(x = 1L) {
    new_object(A, x = x)
  },
  properties = list(
    x = class_integer
  )
)

a = A()

debugonce(str)
str(a)
