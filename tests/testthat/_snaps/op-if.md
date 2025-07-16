# If operator works

    Code
      f
    Output
      func.func @main (%x: tensor<i1>) -> tensor<f32> {
      %3 ="stablehlo.if"(%x)({
        ^bb0(%x1: tensor<f32>):
          %1 ="stablehlo.add"(%x1, %x1):(tensor<f32>, tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%1):(tensor<f32>) -> ()
      }, {
        ^bb0(%x2: tensor<f32>):
          %2 ="stablehlo.abs"(%x2):(tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%2):(tensor<f32>) -> ()
      }):(tensor<i1>) -> (tensor<f32>)
      "func.return"(%3):(tensor<f32>) -> ()
      }

