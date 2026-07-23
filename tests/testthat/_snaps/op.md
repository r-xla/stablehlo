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
      repr(ScalarAttr(name = "dim", value = 42L, dtype = as_dtype("i32")))
    Output
      [1] "dim = 42 : i32"
    Code
      repr(ScalarAttr(name = "size", value = 100L, dtype = as_dtype("i64")))
    Output
      [1] "size = 100 : i64"
    Code
      repr(ScalarAttr(name = "count", value = 255L, dtype = as_dtype("ui8")))
    Output
      [1] "count = 255 : ui8"
    Code
      repr(ScalarAttr(name = "scale", value = 1.5, dtype = as_dtype("f32")))
    Output
      [1] "scale = 1.50000000e+00 : f32"
    Code
      repr(ScalarAttr(name = "rate", value = 0.001, dtype = as_dtype("f64")))
    Output
      [1] "rate = 1.0000000000000000e-03 : f64"
    Code
      repr(ScalarAttr(name = "is_stable", value = TRUE, dtype = as_dtype("bool")))
    Output
      [1] "is_stable = true : i1"
    Code
      repr(ScalarAttr(name = "lower", value = FALSE, dtype = as_dtype("bool")))
    Output
      [1] "lower = false : i1"

# CustomOpBackendConfig repr works correctly

    Code
      repr(CustomOpBackendConfig(list(BoolAttr(name = "flag", value = TRUE),
      ScalarAttr(name = "count", value = 42L, dtype = as_dtype("i32")), StringAttr(
        name = "name", value = "test"))))
    Output
      [1] "backend_config = {\n    flag = true,\n    count = 42 : i32,\n    name = \"test\"\n  }"
    Code
      repr(CustomOpBackendConfig())
    Output
      [1] "backend_config = {}"

