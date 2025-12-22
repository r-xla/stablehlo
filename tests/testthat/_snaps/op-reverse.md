# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x2xf32>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.reverse\" (%x) {\ndimensions = array<i64: 1, 0>\n}: (tensor<2x3x2xf32>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%0): (tensor<2x3x2xf32>) -> ()\n}\n"

