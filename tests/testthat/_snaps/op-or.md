# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xi1>, %y: tensor<3x3xi1>) -> tensor<3x3xi1> {\n%0 = \"stablehlo.or\" (%x, %y): (tensor<3x3xi1>, tensor<3x3xi1>) -> (tensor<3x3xi1>)\n\"func.return\"(%0): (tensor<3x3xi1>) -> ()\n}\n"

