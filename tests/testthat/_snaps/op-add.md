# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>, %y: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.add\" (%x, %y): (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

# error

    Code
      hlo_add(x, y)
    Condition
      Error in `hlo_add()`:
      ! Expected all arguments to have the same tensor type.
      i Got lhs=tensor<2x2xf32>, rhs=tensor<f32>.

