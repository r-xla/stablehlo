# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<15x1x3x3x2x2xf32>, %y: tensor<15x1x3x3x2x2xf32>) -> tensor<15x1x3x3x2x2xf32> {\n%1 = \"stablehlo.remainder\" (%x, %y): (tensor<15x1x3x3x2x2xf32>, tensor<15x1x3x3x2x2xf32>) -> (tensor<15x1x3x3x2x2xf32>)\n\"func.return\"(%1): (tensor<15x1x3x3x2x2xf32>) -> ()\n}\n"

