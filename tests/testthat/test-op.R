test_that("BoolAttr repr works correctly", {
  expect_snapshot({
    repr(BoolAttr(name = "flag", value = TRUE))
    repr(BoolAttr(name = "enabled", value = FALSE))
  })
})

test_that("StringAttr repr works correctly", {
  expect_snapshot({
    repr(StringAttr(name = "target", value = "my_function"))
    repr(StringAttr(name = "name", value = "hello world"))
  })
})

test_that("ScalarAttr repr works correctly", {
  expect_snapshot({
    # Integer types
    repr(ScalarAttr(name = "dim", value = 42L, dtype = IntegerType(32L)))
    repr(ScalarAttr(name = "size", value = 100L, dtype = IntegerType(64L)))

    # Unsigned types
    repr(ScalarAttr(name = "count", value = 255L, dtype = UnsignedType(8L)))

    # Float types
    repr(ScalarAttr(name = "scale", value = 1.5, dtype = FloatType(32L)))
    repr(ScalarAttr(name = "rate", value = 0.001, dtype = FloatType(64L)))

    # Boolean type
    repr(ScalarAttr(name = "is_stable", value = TRUE, dtype = BooleanType()))
    repr(ScalarAttr(name = "lower", value = FALSE, dtype = BooleanType()))
  })
})

test_that("CustomOpBackendConfig repr works correctly", {
  expect_snapshot({
    repr(CustomOpBackendConfig(list(
      BoolAttr(name = "flag", value = TRUE),
      ScalarAttr(name = "count", value = 42L, dtype = IntegerType(32L)),
      StringAttr(name = "name", value = "test")
    )))
    repr(CustomOpBackendConfig())
  })
})

test_that("CustomOpBackendConfig validates element types", {
  expect_error(
    CustomOpBackendConfig(list(1, 2, 3)),
    "Expected item to be a BoolAttr"
  )
  expect_error(
    CustomOpBackendConfig(list(BoolAttr(name = "a", value = TRUE), 2)),
    "Expected item to be a BoolAttr"
  )
})

test_that("CustomOpBackendConfig validates names are unique", {
  expect_error(
    CustomOpBackendConfig(list(
      BoolAttr(name = "a", value = TRUE),
      BoolAttr(name = "a", value = FALSE)
    )),
    "must be unique"
  )
})
