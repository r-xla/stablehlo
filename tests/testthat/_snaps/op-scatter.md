# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%input: tensor<2x3x4x2xi64>, %scatter_indices: tensor<2x2x3x2xi64>, %update: tensor<2x2x3x2x2xi64>) -> tensor<2x3x4x2xi64> {\n%0 = \"stablehlo.scatter\" (%input, %scatter_indices, %update)({\n  ^bb0(%a: tensor<i64>, %b: tensor<i64>):\n    %1 = \"stablehlo.add\" (%a, %b): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    \"stablehlo.return\"(%1): (tensor<i64>) -> ()\n}) {\n  scatter_dimension_numbers = #stablehlo.scatter<\n    update_window_dims = [3, 4],\n    inserted_window_dims = [1],\n    input_batching_dims = [0],\n    scatter_indices_batching_dims = [1],\n    scatter_dims_to_operand_dims = [2, 1],\n    index_vector_dim = 3>,\n  indices_are_sorted = false,\n  unique_indices = false\n}: (tensor<2x3x4x2xi64>, tensor<2x2x3x2xi64>, tensor<2x2x3x2x2xi64>) -> (tensor<2x3x4x2xi64>)\n\"func.return\"(%0): (tensor<2x3x4x2xi64>) -> ()\n}\n"

