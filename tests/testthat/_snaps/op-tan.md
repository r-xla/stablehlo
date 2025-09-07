# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf64>) -> tensor<3x3xf64> {\n%0 = \"stablehlo.tan\" (%x): (tensor<3x3xf64>) -> (tensor<3x3xf64>)\n\"func.return\"(%0): (tensor<3x3xf64>) -> ()\n}\n"

