# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x4xi8>) -> tensor<2x3xui32> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<2x3x4xi8>) -> (tensor<2x3xui32>)\n\"func.return\"(%0): (tensor<2x3xui32>) -> ()\n}\n"

---

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xui32>) -> tensor<2x3xf32> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<2x3xui32>) -> (tensor<2x3xf32>)\n\"func.return\"(%0): (tensor<2x3xf32>) -> ()\n}\n"

---

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf64>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<2x3xf64>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%0): (tensor<2x3x2xf32>) -> ()\n}\n"

