# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<1x3xf32>) -> tensor<2x1x3xf32> {\n%0 = \"stablehlo.broadcast_in_dim\" (%x) {\nbroadcast_dimensions = array<i64: 0, 2>\n}: (tensor<1x3xf32>) -> (tensor<2x1x3xf32>)\n\"func.return\"(%0): (tensor<2x1x3xf32>) -> ()\n}\n"

