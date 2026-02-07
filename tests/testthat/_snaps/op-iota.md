# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main () -> tensor<3x2xi32> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 0 : i64\n}: () -> (tensor<3x2xi32>)\n\"func.return\"(%0): (tensor<3x2xi32>) -> ()\n}\n"

# errors

    Code
      infer_types_iota(scnst(5L, "i64"), dtype = "i32", shape = c(3L, 2L))
    Condition
      Error in `infer_types_iota()`:
      ! `iota_dimension` contains index outside the valid range.
      x Got 5, but valid range is [0, 2).

