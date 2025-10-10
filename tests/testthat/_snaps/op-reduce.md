# basic tests

    Code
      func
    Output
      func.func @main (%x: tensor<2x3xf32>) -> tensor<2xf32> {
      %0 = "stablehlo.constant" () {
      value = dense<0.00000000e+00> : tensor<f32>
      }: () -> (tensor<f32>)
      %1 = "stablehlo.reduce" (%x, %0)({
        ^bb0(%a: tensor<f32>, %b: tensor<f32>):
          %2 = "stablehlo.add" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%2): (tensor<f32>) -> ()
      }) {
      dimensions = array<i64: 1>
      }: (tensor<2x3xf32>, tensor<f32>) -> (tensor<2xf32>)
      "func.return"(%1): (tensor<2xf32>) -> ()
      }

---

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n    \"stablehlo.return\"(%2): (tensor<f32>) -> ()\n}) {\ndimensions = array<i64: 1>\n}: (tensor<2x3xf32>, tensor<f32>) -> (tensor<2xf32>)\n\"func.return\"(%1): (tensor<2xf32>) -> ()\n}\n"

