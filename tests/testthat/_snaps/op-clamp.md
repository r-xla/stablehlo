# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%lo: tensor<2x2xf32>, %x: tensor<2x2xf32>, %hi: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.clamp\" (%lo, %x, %hi): (tensor<2x2xf32>, tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

# scalar min and max

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<5.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%2 = \"stablehlo.clamp\" (%0, %x, %1): (tensor<f32>, tensor<2x2xf32>, tensor<f32>) -> (tensor<2x2xf32>)\n\"func.return\"(%2): (tensor<2x2xf32>) -> ()\n}\n"

