# basic tests

    Code
      repr(result_func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.after_all\" (%x): (tensor<2x2xf32>) -> (!stablehlo.token)\n\"func.return\"(%x): (tensor<2x2xf32>) -> ()\n}\n"

