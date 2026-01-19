# scatter: errors when updates rank is wrong

    Code
      func <- local_func()
      input <- hlo_input("input", "i32", c(3L))
      scatter_indices <- hlo_input("scatter_indices", "i32", c(2L))
      update <- hlo_input("update", "i32", c(2L))
      update_func <- local_func("update")
      a <- hlo_input("a", "i32", integer())
      b <- hlo_input("b", "i32", integer())
      sum_result <- hlo_add(a, b)
      update_func <- hlo_return(sum_result)
      scatter_dim_numbers <- ScatterDimensionNumbers(update_window_dims = 1L,
        inserted_window_dims = integer(), scatter_dims_to_operand_dims = 0L,
        index_vector_dim = 1L)
      hlo_scatter(inputs = list(input), scatter_indices = scatter_indices, updates = list(
        update), scatter_dimension_numbers = scatter_dim_numbers, update_computation = update_func)
    Condition
      Error:
      ! `update_window_dims` contains index outside the valid range.
      x Got 1, but valid range is [0, 1).

# scatter: looks correct in snapshot

    Code
      repr(func)
    Output
      [1] "func.func @main (%input: tensor<4xi32>, %scatter_indices: tensor<2x1xi32>, %update: tensor<2xi32>) -> tensor<4xi32> {\n%0 = \"stablehlo.scatter\"(%input, %scatter_indices, %update)({\n  ^bb0(%a: tensor<i32>, %b: tensor<i32>):\n    %1 = \"stablehlo.add\" (%a, %b): (tensor<i32>, tensor<i32>) -> (tensor<i32>)\n    \"stablehlo.return\"(%1): (tensor<i32>) -> ()\n}) {\nscatter_dimension_numbers = #stablehlo.scatter<update_window_dims = [], inserted_window_dims = [0], scatter_dims_to_operand_dims = [0], index_vector_dim = 1>,\nindices_are_sorted = false, unique_indices = false\n}: (tensor<4xi32>, tensor<2x1xi32>, tensor<2xi32>) -> (tensor<4xi32>)\n\"func.return\"(%0): (tensor<4xi32>) -> ()\n}\n"

