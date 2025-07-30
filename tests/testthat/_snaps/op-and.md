# and works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xi32>, %y: tensor<2x2xi32>) -> tensor<2x2xi32> {\n%1 =\"stablehlo.and\"(%x, %y):(tensor<2x2xi32>, tensor<2x2xi32>) -> (tensor<2x2xi32>)\n\"func.return\"(%1):(tensor<2x2xi32>) -> ()\n}\n"

