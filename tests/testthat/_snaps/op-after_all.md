# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.after_all\" (): () -> (!stablehlo.token)\n%1 = stablehlo.after_all %0 : !stablehlo.token\nreturn %x : tensor<2x2xf32>\n}\n"

