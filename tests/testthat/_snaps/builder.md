# builder works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.add\" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

