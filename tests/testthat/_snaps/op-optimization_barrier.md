# optimization_barrier looks correct

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>, %y: tensor<2x3xf32>) -> (tensor<2x3xf32>, tensor<2x3xf32>) {\n%0, %1 = \"stablehlo.optimization_barrier\" (%x, %y): (tensor<2x3xf32>, tensor<2x3xf32>) -> (tensor<2x3xf32>, tensor<2x3xf32>)\nreturn %0, %1 : tensor<2x3xf32>, tensor<2x3xf32>\n}\n"

