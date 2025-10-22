# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x1: tensor<3x1xi32>, %x2: tensor<3x2xi32>, %x3: tensor<3x3xi32>) -> tensor<3x6xi32> {\n%0 = \"stablehlo.concatenate\" (%x1, %x2, %x3) {\ndimension = 1 : i64 \n}: (tensor<3x1xi32>, tensor<3x2xi32>, tensor<3x3xi32>) -> (tensor<3x6xi32>)\n\"func.return\"(%0): (tensor<3x6xi32>) -> ()\n}\n"

