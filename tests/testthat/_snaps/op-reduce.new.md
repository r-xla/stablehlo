# reduce with addition prints correctly

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<1x6xi64>) -> tensor<1xi64> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i64>\n}: () -> (tensor<i64>)\n%1 = \"stablehlo.reduce\" (%x, %0)({\n  ^bb0(%a: tensor<i64>, %b: tensor<i64>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    \"stablehlo.return\"(%2): (tensor<i64>) -> ()\n}) {\ndimensions = array<i64: 1>\n}: (tensor<1x6xi64>, tensor<i64>) -> (tensor<1xi64>)\n\"func.return\"(%1): (tensor<1xi64>) -> ()\n}\n"

# reduce with addition executes

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n    \"stablehlo.return\"(%2): (tensor<f32>) -> ()\n}) {\ndimensions = array<i64: 1>\n}: (tensor<2x3xf32>, tensor<f32>) -> (tensor<2xf32>)\n\"func.return\"(%1): (tensor<2xf32>) -> ()\n}\n"

