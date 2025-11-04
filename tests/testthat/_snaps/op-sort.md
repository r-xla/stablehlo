# basic tests

    Code
      lapply(f, repr)
    Output
      [[1]]
      [1] "func.func @main (%x1: tensor<2x3xi32>, %x2: tensor<2x3xi32>) -> tensor<2x3xi32> {\n%0, %1 = \"stablehlo.sort\" (%x1, %x2)({\n  ^bb0(%arg1: tensor<i32>, %arg2: tensor<i32>, %arg3: tensor<i32>, %arg4: tensor<i32>):\n    %2 = stablehlo.compare GT, %arg1, %arg2, SIGNED : (tensor<i32>, tensor<i32>) -> (tensor<i1>)\n    \"stablehlo.return\"(%2): (tensor<i1>) -> ()\n}) {\ndimension = 0 :i64,\nis_stable = true\n} : (tensor<2x3xi32>, tensor<2x3xi32>) -> (tensor<2x3xi32>, tensor<2x3xi32>)\n\"func.return\"(%0): (tensor<2x3xi32>) -> ()\n}\n"
      
      [[2]]
      [1] "func.func @main (%x1: tensor<2x3xi32>, %x2: tensor<2x3xi32>) -> tensor<2x3xi32> {\n%0, %1 = \"stablehlo.sort\" (%x1, %x2)({\n  ^bb0(%arg1: tensor<i32>, %arg2: tensor<i32>, %arg3: tensor<i32>, %arg4: tensor<i32>):\n    %2 = stablehlo.compare GT, %arg1, %arg2, SIGNED : (tensor<i32>, tensor<i32>) -> (tensor<i1>)\n    \"stablehlo.return\"(%2): (tensor<i1>) -> ()\n}) {\ndimension = 0 :i64,\nis_stable = true\n} : (tensor<2x3xi32>, tensor<2x3xi32>) -> (tensor<2x3xi32>, tensor<2x3xi32>)\n\"func.return\"(%1): (tensor<2x3xi32>) -> ()\n}\n"
      

