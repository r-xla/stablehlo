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

test_that("TensorDataType equality", {
  expect_true(BooleanType() == BooleanType())
  expect_false(BooleanType() != BooleanType())

  expect_true(IntegerType(32) == IntegerType(32))
  expect_false(IntegerType(32) != IntegerType(32))
  expect_false(IntegerType(32) == IntegerType(64))
  expect_true(IntegerType(32) != IntegerType(64))

  expect_true(UnsignedType(32) == UnsignedType(32))
  expect_false(UnsignedType(32) != UnsignedType(32))
  expect_false(UnsignedType(32) == UnsignedType(64))
  expect_true(UnsignedType(32) != UnsignedType(64))

  expect_true(FloatType(32) == FloatType(32))
  expect_false(FloatType(32) != FloatType(32))
  expect_false(FloatType(32) == FloatType(64))
  expect_true(FloatType(32) != FloatType(64))

  expect_false(IntegerType(32) == FloatType(32))
  expect_true(IntegerType(32) != FloatType(32))
  expect_false(BooleanType() == IntegerType(32))
})

test_that("TensorDataType comparison with character", {
  expect_true(FloatType(32) == "f32")
  expect_true("f32" == FloatType(32))
  expect_false(FloatType(32) != "f32")
  expect_false("f32" != FloatType(32))

  expect_false(FloatType(32) == "f64")
  expect_false("f64" == FloatType(32))
  expect_true(FloatType(32) != "f64")
  expect_true("f64" != FloatType(32))

  expect_true(IntegerType(32) == "i32")
  expect_true("i32" == IntegerType(32))
  expect_false(IntegerType(32) == "i64")

  expect_true(UnsignedType(16) == "ui16")
  expect_true("ui16" == UnsignedType(16))
  expect_false(UnsignedType(16) == "ui32")

  expect_true(BooleanType() == "i1")
  expect_true("i1" == BooleanType())
  expect_false(BooleanType() == "f32")

  expect_false(FloatType(32) == "i32")
  expect_false("i32" == FloatType(32))
})

test_that("TensorType equality", {
  t1 <- TensorType(dtype = FloatType(32), shape = Shape(c(2, 3)))
  t2 <- TensorType(dtype = FloatType(32), shape = Shape(c(2, 3, 1)))
  t3 <- TensorType(dtype = IntegerType(32), shape = Shape(c(2, 3)))
  t4 <- TensorType(dtype = IntegerType(32), shape = Shape(c()))

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

test_that("print methods - data types", {
  expect_snapshot({
    BooleanType()
  })

  expect_snapshot({
    IntegerType(8)
    IntegerType(16)
    IntegerType(32)
    IntegerType(64)
  })

  expect_snapshot({
    UnsignedType(8)
    UnsignedType(16)
    UnsignedType(32)
    UnsignedType(64)
  })

  expect_snapshot({
    FloatType(32)
    FloatType(64)
  })
})

test_that("print methods - compound types", {
  expect_snapshot({
    Shape(c())
    Shape(c(5))
    Shape(c(2, 3, 4))
    Shape(c(10, NA, 20))
  })

  expect_snapshot({
    TensorType(BooleanType(), Shape(c()))
    TensorType(IntegerType(32), Shape(c(10)))
    TensorType(FloatType(64), Shape(c(2, 3, 4)))
    TensorType(UnsignedType(16), Shape(c(5, 6)))
  })

  expect_snapshot({
    TokenType()
  })

  expect_snapshot({
    ValueTypes(list())
  })

  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(IntegerType(32), Shape(c(2))))
    ))
  })

  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(IntegerType(32), Shape(c(2)))),
      ValueType(TensorType(FloatType(32), Shape(c(3)))),
      ValueType(TensorType(BooleanType(), Shape(c())))
    ))
  })
})
