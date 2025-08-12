# atan2 works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<f32>, %y: tensor<f32>) -> tensor<f32> {\n%1 = \"stablehlo.atan2\" (%x, %y): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n\"func.return\"(%1): (tensor<f32>) -> ()\n}\n"

