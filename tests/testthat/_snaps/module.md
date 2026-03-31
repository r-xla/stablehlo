# Module repr with multiple functions

    Code
      repr(mod)
    Output
      [1] "module {\n  func.func @forward (%a: tensor<2x2xf32>, %b: tensor<2x2xf32>) -> tensor<2x2xf32> {\n  %0 = stablehlo.add %a, %b : tensor<2x2xf32>\n  return %0 : tensor<2x2xf32>\n  }\n  func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n  %1 = func.call @forward(%x, %y) : (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n  return %1 : tensor<2x2xf32>\n  }\n}\n"

# Module with single function

    Code
      repr(mod)
    Output
      [1] "module {\n  func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n  return %x : tensor<2x2xf32>\n  }\n}\n"

