# integer to float conversion

    Code
      repr(f)
    Output
      [1] "func.func @main (%operand: tensor<4xi32>) -> tensor<4xf32> {\n%0 = \"stablehlo.convert\" (%operand): (tensor<4xi32>) -> (tensor<4xf32>)\n\"func.return\"(%0): (tensor<4xf32>) -> ()\n}\n"

