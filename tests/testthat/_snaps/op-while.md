# while operator works

    Code
      repr(func)
    Output
      [1] "func.func @main (%init_i: tensor<i64>, %init_sum: tensor<i64>) -> (tensor<i64>, tensor<i64>) {\n%0, %1 = \"stablehlo.while\" (%init_i, %init_sum)({\n  ^bb0(%arg0: tensor<i64>, %arg1: tensor<i64>):\n    %2 = \"stablehlo.constant\" () {\nvalue = dense<10> : tensor<i64>\n}: () -> (tensor<i64>)\n    %3 = stablehlo.compare LT, %arg0, %2, SIGNED : (tensor<i64>, tensor<i64>) -> (tensor<i1>)\n    \"stablehlo.return\"(%3): (tensor<i1>) -> ()\n}, {\n  ^bb0(%arg0: tensor<i64>, %arg1: tensor<i64>):\n    %4 = \"stablehlo.constant\" () {\nvalue = dense<1> : tensor<i64>\n}: () -> (tensor<i64>)\n    %5 = \"stablehlo.add\" (%arg1, %4): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    %6 = \"stablehlo.add\" (%arg0, %4): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    \"stablehlo.return\"(%6, %5): (tensor<i64>, tensor<i64>) -> ()\n}): (tensor<i64>, tensor<i64>) -> (tensor<i64>, tensor<i64>)\n\"func.return\"(%0, %1): (tensor<i64>, tensor<i64>) -> ()\n}\n"

