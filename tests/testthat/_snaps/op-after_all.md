# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.after_all\" (): () -> (!stablehlo.token)\n%1 = stablehlo.after_all %0 : !stablehlo.token\nreturn %x : tensor<2x2xf32>\n}\n"

# a non-token input is rejected

    Code
      hlo_after_all(x)
    Condition
      Error:
      ! `inputs[0]` must be tokens.
      x Got tensor<2x2xf32>.

