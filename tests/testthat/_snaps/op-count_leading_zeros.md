# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2xi32>) -> tensor<2xi32> {\n%0 = \"stablehlo.count_leading_zeros\" (%x): (tensor<2xi32>) -> (tensor<2xi32>)\n\"func.return\"(%0): (tensor<2xi32>) -> ()\n}\n"

