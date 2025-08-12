# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%lo: tensor<2x2xf32>, %x: tensor<2x2xf32>, %hi: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%1 =\"stablehlo.clamp\"(%lo, %x, %hi):(tensor<2x2xf32>, tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%1):(tensor<2x2xf32>) -> ()\n}\n"

