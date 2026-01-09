# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4x4xi32>) -> tensor<2x2xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<-1> : tensor<i64>\n}: () -> (tensor<i64>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i64>\n}: () -> (tensor<i64>)\n%2 = \"stablehlo.dynamic_slice\" (%x, %0, %1) {\nslice_sizes = array<i64: 2, 2>\n}: (tensor<4x4xi32>, tensor<i64>, tensor<i64>) -> (tensor<2x2xi32>)\n\"func.return\"(%2): (tensor<2x2xi32>) -> ()\n}\n"

