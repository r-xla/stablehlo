# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf32>) -> tensor<3x3xi1> {\n%0 = \"chlo.is_inf\" (%x): (tensor<3x3xf32>) -> (tensor<3x3xi1>)\nreturn %0 : tensor<3x3xi1>\n}\n"

