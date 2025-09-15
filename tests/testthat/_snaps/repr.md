# same constant gets same id

    Code
      repr(f)
    Output
      [1] "func.func @main () -> (tensor<i32>, tensor<i32>, tensor<i32>) {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<1> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<1> : tensor<i32>\n}: () -> (tensor<i32>)\n\"func.return\"(%0, %0, %1): (tensor<i32>, tensor<i32>, tensor<i32>) -> ()\n}\n"

