# basic reduce_window (sum pooling)

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<4x4xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce_window\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n    \"stablehlo.return\"(%2): (tensor<f32>) -> ()\n}) {\nwindow_dimensions = array<i64: 2, 2>,\nwindow_strides = array<i64: 2, 2>\n}: (tensor<4x4xf32>, tensor<f32>) -> (tensor<2x2xf32>)\n\"func.return\"(%1): (tensor<2x2xf32>) -> ()\n}\n"

