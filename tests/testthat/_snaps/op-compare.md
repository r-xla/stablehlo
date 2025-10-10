# simple test

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<f32>, %rhs: tensor<f32>) -> tensor<i1> {\n%0 = stablehlo.compare LT, %lhs, %rhs, FLOAT : (tensor<f32>, tensor<f32>) -> (tensor<i1>)\n\"func.return\"(%0): (tensor<i1>) -> ()\n}\n"

