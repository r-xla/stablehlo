# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main () -> tensor<3x2xi32> {\n%0 = \"stablehlo.iota\" () {\niota_dimension = 0 : i64\n}: () -> (tensor<3x2xi32>)\n\"func.return\"(%0): (tensor<3x2xi32>) -> ()\n}\n"

