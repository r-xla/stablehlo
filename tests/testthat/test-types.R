test_that("repr.DataType", {
  expect_equal(repr(as_dtype("f32")), "f32")
  expect_equal(repr(as_dtype("bool")), "i1")
})

test_that("repr.DataType uses MLIR spellings", {
  expect_equal(repr(as_dtype("f8e5m2")), "f8E5M2")
  expect_equal(repr(as_dtype("f8e4m3fn")), "f8E4M3FN")
  expect_equal(repr(as_dtype("f8e8m0fnu")), "f8E8M0FNU")
  expect_equal(repr(as_dtype("f4e2m1fn")), "f4E2M1FN")
  expect_equal(repr(as_dtype("c64")), "complex<f32>")
  expect_equal(repr(as_dtype("c128")), "complex<f64>")
  expect_equal(repr(as_dtype("bf16")), "bf16")
})

test_that("TensorType repr", {
  tt <- TensorType(
    dtype = as_dtype("f32"),
    shape = Shape(c(1L, 2L))
  )
  expect_equal(repr(tt), "tensor<1x2xf32>")

  expect_equal(
    repr(TensorType(dtype = as_dtype("f8e5m2"), shape = Shape(4L))),
    "tensor<4xf8E5M2>"
  )
})

test_that("ValueType rejects a bare DataType", {
  expect_error(
    ValueType(as_dtype("f32")),
    "must be a TensorType or TokenType"
  )
})

test_that("is_dtype", {
  expect_true(is_dtype(as_dtype("bool")))
  expect_false(is_dtype("i32"))
})

test_that("as_dtype", {
  expect_equal(as_dtype("f32"), as_dtype("f32"))
})

test_that("TensorType equality", {
  t1 <- TensorType(dtype = as_dtype("f32"), shape = Shape(c(2, 3)))
  t2 <- TensorType(dtype = as_dtype("f32"), shape = Shape(c(2, 3, 1)))
  t3 <- TensorType(dtype = as_dtype("i32"), shape = Shape(c(2, 3)))
  t4 <- TensorType(dtype = as_dtype("i32"), shape = Shape(c()))

  expect_true(t1 == t1)
  expect_false(t1 != t1)

  expect_false(t1 == t2)
  expect_true(t1 != t2)

  expect_true(t3 == t3)
  expect_false(t3 != t3)

  expect_false(t3 == t4)
  expect_true(t3 != t4)

  expect_true(t1 != t4)
  expect_true(t1 != t3)

  expect_false(t1 == t4)
  expect_false(t1 == t3)
})

test_that("print methods - compound types", {
  expect_snapshot({
    Shape(c())
    Shape(c(5))
    Shape(c(2, 3, 4))
    Shape(c(10, NA, 20))
  })

  expect_snapshot({
    TensorType(as_dtype("bool"), Shape(c()))
    TensorType(as_dtype("i32"), Shape(c(10)))
    TensorType(as_dtype("f64"), Shape(c(2, 3, 4)))
    TensorType(as_dtype("ui16"), Shape(c(5, 6)))
  })

  expect_snapshot({
    TokenType()
  })

  expect_snapshot({
    ValueTypes(list())
  })

  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(as_dtype("i32"), Shape(c(2))))
    ))
  })

  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(as_dtype("i32"), Shape(c(2)))),
      ValueType(TensorType(as_dtype("f32"), Shape(c(3)))),
      ValueType(TensorType(as_dtype("bool"), Shape(c())))
    ))
  })
})
