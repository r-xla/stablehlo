# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.cbrt\" (%x): (tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

