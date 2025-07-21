# builder works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%1 =\"stablehlo.add\"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%1):(tensor<2x2xf32>) -> ()\n}\n"

