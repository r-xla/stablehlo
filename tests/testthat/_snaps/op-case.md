# Case operator works

    Code
      f
    Output
      func.func @main (%index: tensor<i32>, %x1: tensor<2xi64>, %x2: tensor<2xi64>) -> (tensor<2xi64>, tensor<2xi64>) {
      %0, %1 = "stablehlo.case" (%index)({
      "stablehlo.return"(%x1, %x1): (tensor<2xi64>, tensor<2xi64>) -> ()
      }, {
      "stablehlo.return"(%x2, %x2): (tensor<2xi64>, tensor<2xi64>) -> ()
      }): (tensor<i32>) -> (tensor<2xi64>, tensor<2xi64>)
      "func.return"(%0, %1): (tensor<2xi64>, tensor<2xi64>) -> ()
      }

