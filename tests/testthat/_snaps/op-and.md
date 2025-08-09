# and works

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<i1>, %y: tensor<i1>) -> tensor<i1> {\n%1 = \"stablehlo.and\" (%x, %y): (tensor<i1>, tensor<i1>) -> (tensor<i1>)\n\"func.return\"(%1): (tensor<i1>) -> ()\n}\n"

