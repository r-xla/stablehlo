# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xi32>, %y: tensor<3x3xi32>) -> tensor<3x3xi32> {\n%0 = \"stablehlo.shift_right_arithmetic\" (%x, %y): (tensor<3x3xi32>, tensor<3x3xi32>) -> (tensor<3x3xi32>)\n\"func.return\"(%0): (tensor<3x3xi32>) -> ()\n}\n"

