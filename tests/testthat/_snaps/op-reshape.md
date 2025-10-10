# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x2xf32>) -> tensor<4x3xf32> {\n%0 = \"stablehlo.reshape\" (%x): (tensor<2x3x2xf32>) -> (tensor<4x3xf32>)\n\"func.return\"(%0): (tensor<4x3xf32>) -> ()\n}\n"

