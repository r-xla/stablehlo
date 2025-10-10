# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%pred: tensor<2x3x2xi1>, %on_true: tensor<2x3x2xf32>, %on_false: tensor<2x3x2xf32>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.select\" (%pred, %on_true, %on_false): (tensor<2x3x2xi1>, tensor<2x3x2xf32>, tensor<2x3x2xf32>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%0): (tensor<2x3x2xf32>) -> ()\n}\n"

