# BoolAttr repr works correctly

    Code
      repr(BoolAttr(name = "flag", value = TRUE))
    Output
      [1] "flag = true"
    Code
      repr(BoolAttr(name = "enabled", value = FALSE))
    Output
      [1] "enabled = false"

# StringAttr repr works correctly

    Code
      repr(StringAttr(name = "target", value = "my_function"))
    Output
      [1] "target = \"my_function\""
    Code
      repr(StringAttr(name = "name", value = "hello world"))
    Output
      [1] "name = \"hello world\""

# ScalarAttr repr works correctly

    Code
      repr(ScalarAttr(name = "dim", value = 42L, dtype = IntegerType(32L)))
    Output
      [1] "dim = 42 : i32"
    Code
      repr(ScalarAttr(name = "size", value = 100L, dtype = IntegerType(64L)))
    Output
      [1] "size = 100 : i64"
    Code
      repr(ScalarAttr(name = "count", value = 255L, dtype = UnsignedType(8L)))
    Output
      [1] "count = 255 : ui8"
    Code
      repr(ScalarAttr(name = "scale", value = 1.5, dtype = FloatType(32L)))
    Output
      [1] "scale = 1.50000000e+00 : f32"
    Code
      repr(ScalarAttr(name = "rate", value = 0.001, dtype = FloatType(64L)))
    Output
      [1] "rate = 1.0000000000000000e-03 : f64"
    Code
      repr(ScalarAttr(name = "is_stable", value = TRUE, dtype = BooleanType()))
    Output
      [1] "is_stable = true : i1"
    Code
      repr(ScalarAttr(name = "lower", value = FALSE, dtype = BooleanType()))
    Output
      [1] "lower = false : i1"

# CustomOpBackendConfig repr works correctly

    Code
      repr(CustomOpBackendConfig(list(BoolAttr(name = "flag", value = TRUE),
      ScalarAttr(name = "count", value = 42L, dtype = IntegerType(32L)), StringAttr(
        name = "name", value = "test"))))
    Output
      [1] "backend_config = {\n    flag = true,\n    count = 42 : i32,\n    name = \"test\"\n  }"
    Code
      repr(CustomOpBackendConfig())
    Output
      [1] "backend_config = {}"

