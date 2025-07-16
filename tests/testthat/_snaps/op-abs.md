# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%1 =stablehlo.abs(%x):tensor<2x2xf32> -> tensor<2x2xf32>\nfunc.return(%1):tensor<2x2xf32>\n}\n"

