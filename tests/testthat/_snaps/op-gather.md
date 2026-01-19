# gather: looks correct in snapshot

    Code
      repr(func)
    Output
      [1] "func.func @main (%operand: tensor<3x4xi32>, %start_indices: tensor<2x2xi32>) -> tensor<2x2xi32> {\n%0 = \"stablehlo.gather\"(%operand, %start_indices) {\ndimension_numbers = #stablehlo.gather<offset_dims = [1], collapsed_slice_dims = [0], start_index_map = [0, 1], index_vector_dim = 1>,\nslice_sizes = array<i64: 1, 2>, indices_are_sorted = false\n}: (tensor<3x4xi32>, tensor<2x2xi32>) -> (tensor<2x2xi32>)\n\"func.return\"(%0): (tensor<2x2xi32>) -> ()\n}\n"

