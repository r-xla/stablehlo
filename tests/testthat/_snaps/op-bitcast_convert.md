# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2xi8>) -> tensor<i16> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<2xi8>) -> (tensor<i16>)\n\"func.return\"(%0): (tensor<i16>) -> ()\n}\n"

---

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<i16>) -> tensor<2xi8> {\n%0 = \"stablehlo.bitcast_convert\" (%x): (tensor<i16>) -> (tensor<2xi8>)\n\"func.return\"(%0): (tensor<2xi8>) -> ()\n}\n"

---

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

