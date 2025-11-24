# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2x3x4xui8> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<2x3xf32>) -> (tensor<2x3x4xui8>)\n\"func.return\"(%0): (tensor<2x3x4xui8>) -> ()\n}\n"

