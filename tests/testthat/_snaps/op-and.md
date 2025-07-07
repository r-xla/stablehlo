# and works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xsi32>, %y: tensor<2x2xsi32>) -> tensor<2x2xsi32> {\n%5 =\"stablehlo.and\"(%x, %y):(tensor<2x2xsi32>, tensor<2x2xsi32>) -> (tensor<2x2xsi32>)\n\"stablehlo.return\"(%5):(tensor<2x2xsi32>) -> ()\n}"

