# scatter looks correct

    Code
      repr(func)
    Output
      [1] "func.func @main (%input: tensor<4xi32>, %scatter_indices: tensor<2x1xi32>, %update: tensor<2xi32>) -> tensor<4xi32> {\n%0 = \"stablehlo.scatter\"(%input, %scatter_indices, %update)({\n  ^bb0(%a: tensor<i32>, %b: tensor<i32>):\n    %1 = \"stablehlo.add\" (%a, %b): (tensor<i32>, tensor<i32>) -> (tensor<i32>)\n    \"stablehlo.return\"(%1): (tensor<i32>) -> ()\n}) {\nscatter_dimension_numbers = #stablehlo.scatter<update_window_dims = [], inserted_window_dims = [0], scatter_dims_to_operand_dims = [0], index_vector_dim = 1>,\nindices_are_sorted = false, unique_indices = false\n}: (tensor<4xi32>, tensor<2x1xi32>, tensor<2xi32>) -> (tensor<4xi32>)\n\"func.return\"(%0): (tensor<4xi32>) -> ()\n}\n"

