# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x2xf32>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1, 0]> : tensor<2xi64>\n}: () -> (tensor<2xi64>)\n%1 = \"stablehlo.reverse\" (%x) {\ndimensions = array<i64: 1, 0>\n}: (tensor<2x3x2xf32>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%1): (tensor<2x3x2xf32>) -> ()\n}\n"

