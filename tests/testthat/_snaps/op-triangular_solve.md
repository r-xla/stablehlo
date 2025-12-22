# basic triangular_solve

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose NO_TRANSPOSE>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

# right_side solve

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = false,\nlower = false,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose NO_TRANSPOSE>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

# batched triangular_solve

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<2x3x3xf32>, %b: tensor<2x3x4xf32>) -> tensor<2x3x4xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose NO_TRANSPOSE>\n}: (tensor<2x3x3xf32>, tensor<2x3x4xf32>) -> (tensor<2x3x4xf32>)\n\"func.return\"(%0): (tensor<2x3x4xf32>) -> ()\n}\n"

# transpose_a = TRANSPOSE

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose TRANSPOSE>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

# transpose_a = ADJOINT

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose ADJOINT>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

