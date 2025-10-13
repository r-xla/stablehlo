# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4x3xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.slice\" (%x) {\nstart_indices = array<i64: 2, 1>,\nlimit_indices = array<i64: 4, 3>,\nstrides = array<i64: 1, 1>\n}: (tensor<4x3xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

