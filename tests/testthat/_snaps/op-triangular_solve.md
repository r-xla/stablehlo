# basic triangular_solve

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose NO_TRANSPOSE>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

