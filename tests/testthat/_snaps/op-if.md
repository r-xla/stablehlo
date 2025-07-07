# If operator works

    Code
      f
    Output
      func.func @ (%x: tensor<i1>) -> tensor<f32> {
      %9 ="stablehlo.if"(%x)({
        ^bb0(%x: tensor<f32>)
          %7 ="stablehlo.add"(%x, %x):(tensor<f32>, tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%7):(tensor<f32>) -> ()
      }, {
        ^bb0(%x: tensor<f32>)
          %8 ="stablehlo.abs"(%x):(tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%8):(tensor<f32>) -> ()
      }):(tensor<i1>) -> (tensor<f32>)
      "stablehlo.return"(%9):(tensor<f32>) -> ()
      }

