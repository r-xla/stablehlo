# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main () -> tensor<4x5xi32> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 0 : i64\n}: () -> (tensor<4x5xi32>)\n\"func.return\"(%0): (tensor<4x5xi32>) -> ()\n}\n"

---

    Code
      repr(f1)
    Output
      [1] "func.func @main () -> tensor<4x5xi32> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 1 : i64\n}: () -> (tensor<4x5xi32>)\n\"func.return\"(%0): (tensor<4x5xi32>) -> ()\n}\n"

---

    Code
      repr(f_f)
    Output
      [1] "func.func @main () -> tensor<3x2xf32> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 0 : i64\n}: () -> (tensor<3x2xf32>)\n\"func.return\"(%0): (tensor<3x2xf32>) -> ()\n}\n"

---

    Code
      repr(f_1d)
    Output
      [1] "func.func @main () -> tensor<5xi64> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 0 : i64\n}: () -> (tensor<5xi64>)\n\"func.return\"(%0): (tensor<5xi64>) -> ()\n}\n"

