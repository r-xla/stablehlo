# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.cholesky\" (%x){\nlower = false\n}: (tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

