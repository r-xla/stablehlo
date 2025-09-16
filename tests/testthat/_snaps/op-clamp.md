# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%lo: tensor<2x2xf32>, %x: tensor<2x2xf32>, %hi: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.clamp\" (%lo, %x, %hi): (tensor<2x2xf32>, tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

