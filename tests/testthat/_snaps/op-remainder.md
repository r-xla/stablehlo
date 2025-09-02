# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {\n%1 = \"stablehlo.remainder\" (%x, %y): (tensor<4xf32>, tensor<4xf32>) -> (tensor<4xf32>)\n\"func.return\"(%1): (tensor<4xf32>) -> ()\n}\n"

