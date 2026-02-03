# errors

    Code
      infer_types_reshape(vt("f32", c(2L, 3L)), shape = c(4L, 2L))
    Condition
      Error in `infer_types_reshape()`:
      ! Size of output must equal to size of `operand`

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x2xf32>) -> tensor<4x3xf32> {\n%0 = \"stablehlo.reshape\" (%x): (tensor<2x3x2xf32>) -> (tensor<4x3xf32>)\n\"func.return\"(%0): (tensor<4x3xf32>) -> ()\n}\n"

