# If operator works

    Code
      f
    Output
      func.func @ (%x: tensor<i1>) -> tensor<f32> {
      %3 ="stablehlo.if"(%x)({
        ^bb0(%x: tensor<f32>)
          %1 ="stablehlo.add"(%x, %x):(tensor<f32>, tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%1):(tensor<f32>) -> ()
      }, {
        ^bb0(%x: tensor<f32>)
          %2 ="stablehlo.abs"(%x):(tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%2):(tensor<f32>) -> ()
      }):(tensor<i1>) -> (tensor<f32>)
      "stablehlo.return"(%3):(tensor<f32>) -> ()
      }

