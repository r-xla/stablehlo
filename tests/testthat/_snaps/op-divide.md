# basic tests

    Code
<<<<<<< HEAD
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf64>, %y: tensor<3x3xf64>) -> tensor<3x3xf64> {\n%0 = \"stablehlo.divide\" (%x, %y): (tensor<3x3xf64>, tensor<3x3xf64>) -> (tensor<3x3xf64>)\n\"func.return\"(%0): (tensor<3x3xf64>) -> ()\n}\n"
=======
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {\n%1 =\"stablehlo.divide\"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%1):(tensor<2x2xf32>) -> ()\n}\n"
>>>>>>> 4d95069 (feat: op cosine)

