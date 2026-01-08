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
  # BooleanType
  expect_snapshot({
    BooleanType()
  })

  # IntegerType
  expect_snapshot({
    IntegerType(8)
    IntegerType(16)
    IntegerType(32)
    IntegerType(64)
  })

  # UnsignedType
  expect_snapshot({
    UnsignedType(8)
    UnsignedType(16)
    UnsignedType(32)
    UnsignedType(64)
  })

  # FloatType
  expect_snapshot({
    FloatType(32)
    FloatType(64)
  })
})

test_that("print methods - compound types", {
  # Shape
  expect_snapshot({
    Shape(c())
    Shape(c(5))
    Shape(c(2, 3, 4))
    Shape(c(10, NA, 20))
  })

  # TensorType
  expect_snapshot({
    TensorType(BooleanType(), Shape(c()))
    TensorType(IntegerType(32), Shape(c(10)))
    TensorType(FloatType(64), Shape(c(2, 3, 4)))
    TensorType(UnsignedType(16), Shape(c(5, 6)))
  })

  # TokenType
  expect_snapshot({
    TokenType()
  })

  # ValueType
  expect_snapshot({
    ValueType(TensorType(IntegerType(32), Shape(c())))
    ValueType(TensorType(FloatType(32), Shape(c(10, 20))))
    ValueType(TokenType())
  })

  # ValueTypes - empty
  expect_snapshot({
    ValueTypes(list())
  })

  # ValueTypes - single
  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(IntegerType(32), Shape(c(2))))
    ))
  })

  # ValueTypes - multiple
  expect_snapshot({
    ValueTypes(list(
      ValueType(TensorType(IntegerType(32), Shape(c(2)))),
      ValueType(TensorType(FloatType(32), Shape(c(3)))),
      ValueType(TensorType(BooleanType(), Shape(c())))
    ))
  })
})
