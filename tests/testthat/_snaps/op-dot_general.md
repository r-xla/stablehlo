# batching_dims

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x5x4xf32>, %rhs: tensor<4x3x1xf32>) -> tensor<1x5x3xf32> {\n%21 = stablehlo.dot_general %lhs, %rhs, batching_dims = [0] x [2], contracting_dims = [2] x [0]: (tensor<1x5x4xf32>, tensor<4x3x1xf32>) -> (tensor<1x5x3xf32>)\n\"func.return\"(%21): (tensor<1x5x3xf32>) -> ()\n}\n"

