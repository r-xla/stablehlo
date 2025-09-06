# If operator works

    Code
      f
    Output
      func.func @main (%pred: tensor<i1>, %x1: tensor<f32>, %x2: tensor<f32>) -> tensor<f32> {
      %0 = "stablehlo.if" (%pred)({
      "stablehlo.return"(%x1): (tensor<f32>) -> ()
      }, {
      "stablehlo.return"(%x2): (tensor<f32>) -> ()
      }): (tensor<i1>) -> (tensor<f32>)
      "func.return"(%0): (tensor<f32>) -> ()
      }

