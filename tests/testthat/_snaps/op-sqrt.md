# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<4x23xf32>) -> tensor<4x23xf32> {\n%1 = \"stablehlo.sqrt\" (%x): (tensor<4x23xf32>) -> (tensor<4x23xf32>)\n\"func.return\"(%1): (tensor<4x23xf32>) -> ()\n}\n"

