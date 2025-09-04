# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf64>, %y: tensor<3x3xf64>) -> tensor<3x3xf64> {\n%1 = \"stablehlo.maximum\" (%x, %y): (tensor<3x3xf64>, tensor<3x3xf64>) -> (tensor<3x3xf64>)\n\"func.return\"(%1): (tensor<3x3xf64>) -> ()\n}\n"

