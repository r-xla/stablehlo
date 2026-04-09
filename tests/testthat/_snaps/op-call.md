# hlo_call basic repr

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = func.call @forward(%x, %y) : (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\nreturn %0 : tensor<2x2xf32>\n}\n"

# hlo_call with multiple outputs

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> (tensor<2x2xf32>, tensor<2x2xf32>) {\n%0, %1 = func.call @helper(%x) : (tensor<2x2xf32>) -> (tensor<2x2xf32>, tensor<2x2xf32>)\nreturn %0, %1 : tensor<2x2xf32>, tensor<2x2xf32>\n}\n"

