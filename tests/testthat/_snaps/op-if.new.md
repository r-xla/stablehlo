# If operator works

    Code
      f
    Output
      func.func @main (%x: tensor<i1>) -> tensor<1xf32> {
      %3 ="stablehlo.if"(%x)({
        ^bb0():
          %1 ="stablehlo.constant"(){ value = dense<+1.0000000000000000E+00> : tensor<1xf32> }:() -> (tensor<1xf32>)
          "stablehlo.return"(%1):(tensor<1xf32>) -> ()
      }, {
        ^bb0():
          %2 ="stablehlo.constant"(){ value = dense<+2.0000000000000000E+00> : tensor<1xf32> }:() -> (tensor<1xf32>)
          "stablehlo.return"(%2):(tensor<1xf32>) -> ()
      }):(tensor<i1>) -> (tensor<1xf32>)
      "func.return"(%3):(tensor<1xf32>) -> ()
      }

