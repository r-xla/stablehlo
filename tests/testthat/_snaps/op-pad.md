# basic edge padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<4x5xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 1>,\nedge_padding_high = array<i64: 2, 1>,\ninterior_padding = array<i64: 0, 0>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<4x5xi32>)\n\"func.return\"(%1): (tensor<4x5xi32>) -> ()\n}\n"

# interior padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<3x7xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 0>,\nedge_padding_high = array<i64: 0, 0>,\ninterior_padding = array<i64: 1, 2>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<3x7xi32>)\n\"func.return\"(%1): (tensor<3x7xi32>) -> ()\n}\n"

