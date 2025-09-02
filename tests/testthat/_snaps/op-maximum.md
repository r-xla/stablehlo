# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>, %y: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%1 = \"stablehlo.maximum\" (%x, %y): (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%1): (tensor<3x3xf32>) -> ()\n}\n"

