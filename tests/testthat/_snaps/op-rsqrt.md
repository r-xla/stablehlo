# basic tests

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<21x4x4x2x7x16x9x1x3x2xf32>) -> tensor<21x4x4x2x7x16x9x1x3x2xf32> {\n%1 = \"stablehlo.rsqrt\" (%x): (tensor<21x4x4x2x7x16x9x1x3x2xf32>) -> (tensor<21x4x4x2x7x16x9x1x3x2xf32>)\n\"func.return\"(%1): (tensor<21x4x4x2x7x16x9x1x3x2xf32>) -> ()\n}\n"

