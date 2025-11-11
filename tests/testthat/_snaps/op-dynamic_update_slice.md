# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4x4xf32>, %update: tensor<2x2xf32>) -> tensor<4x4xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<-1> : tensor<i64>\n}: () -> (tensor<i64>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i64>\n}: () -> (tensor<i64>)\n%2 = \"stablehlo.dynamic_update_slice\" (%x, %update, %0, %1): (tensor<4x4xf32>, tensor<2x2xf32>, tensor<i64>, tensor<i64>) -> (tensor<4x4xf32>)\n\"func.return\"(%2): (tensor<4x4xf32>) -> ()\n}\n"

