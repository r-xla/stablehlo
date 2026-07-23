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
    repr(ScalarAttr(name = "dim", value = 42L, dtype = as_dtype("i32")))
    repr(ScalarAttr(name = "size", value = 100L, dtype = as_dtype("i64")))

    # Unsigned types
    repr(ScalarAttr(name = "count", value = 255L, dtype = as_dtype("ui8")))

    # Float types
    repr(ScalarAttr(name = "scale", value = 1.5, dtype = as_dtype("f32")))
    repr(ScalarAttr(name = "rate", value = 0.001, dtype = as_dtype("f64")))

    # Boolean type
    repr(ScalarAttr(name = "is_stable", value = TRUE, dtype = as_dtype("bool")))
    repr(ScalarAttr(name = "lower", value = FALSE, dtype = as_dtype("bool")))
  })
})

test_that("CustomOpBackendConfig repr works correctly", {
  expect_snapshot({
    repr(CustomOpBackendConfig(list(
      BoolAttr(name = "flag", value = TRUE),
      ScalarAttr(name = "count", value = 42L, dtype = as_dtype("i32")),
      StringAttr(name = "name", value = "test")
    )))
    repr(CustomOpBackendConfig())
  })
})

test_that("CustomOpBackendConfig validates data types", {
  expect_error(
    CustomOpBackendConfig(list(1, 2, 3)),
    "May only contain the following types"
  )
  expect_error(
    CustomOpBackendConfig(list(BoolAttr(name = "a", value = TRUE), 2)),
    "May only contain the following types"
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
