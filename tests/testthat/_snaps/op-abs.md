# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = stablehlo.abs %x : tensor<3x3xf32>\nreturn %0 : tensor<3x3xf32>\n}\n"

# unsigned operands are rejected

    Code
      infer_types_abs(vt("ui32", 3L))
    Condition
      Error in `infer_types_abs()`:
      ! `operand` must have dtype float or int.
      x Got ui32.

