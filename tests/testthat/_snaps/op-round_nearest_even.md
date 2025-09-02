# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<5xf32>) -> tensor<5xf32> {\n%1 = \"stablehlo.round_nearest_even\" (%x): (tensor<5xf32>) -> (tensor<5xf32>)\n\"func.return\"(%1): (tensor<5xf32>) -> ()\n}\n"

