# atan2 works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%6 =\"stablehlo.atan2\"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"stablehlo.return\"(%6):(tensor<2x2xf32>) -> ()\n}"

