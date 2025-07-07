# abs works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%2 =\"stablehlo.abs\"(%x):(tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"stablehlo.return\"(%2):(tensor<2x2xf32>) -> ()\n}"

