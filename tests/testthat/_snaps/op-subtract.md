# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>, %y: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.subtract\" (%x, %y): (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

