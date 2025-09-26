# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x4xf32>) -> tensor<4x2x3xf32> {\n%0 = \"stablehlo.transpose\" (%x) {\npermutation = array<i64: 2, 0, 1>\n}: (tensor<2x3x4xf32>) -> (tensor<4x2x3xf32>)\n\"func.return\"(%0): (tensor<4x2x3xf32>) -> ()\n}\n"

