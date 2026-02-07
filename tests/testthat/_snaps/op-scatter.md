# scatter looks correct

    Code
      repr(func)
    Output
      [1] "func.func @main (%input: tensor<4xi32>, %scatter_indices: tensor<2x1xi32>, %update: tensor<2xi32>) -> tensor<4xi32> {\n%0 = \"stablehlo.scatter\"(%input, %scatter_indices, %update)({\n  ^bb0(%a: tensor<i32>, %b: tensor<i32>):\n    %1 = \"stablehlo.add\" (%a, %b): (tensor<i32>, tensor<i32>) -> (tensor<i32>)\n    \"stablehlo.return\"(%1): (tensor<i32>) -> ()\n}) {\nscatter_dimension_numbers = #stablehlo.scatter<update_window_dims = [], inserted_window_dims = [0], scatter_dims_to_operand_dims = [0], index_vector_dim = 1>,\nindices_are_sorted = false, unique_indices = false\n}: (tensor<4xi32>, tensor<2x1xi32>, tensor<2xi32>) -> (tensor<4xi32>)\n\"func.return\"(%0): (tensor<4xi32>) -> ()\n}\n"

# errors

    Code
      infer_types_scatter(inputs = inputs, scatter_indices = scatter_indices,
        updates = updates, scatter_dimension_numbers = sdn, indices_are_sorted = scnst(
          FALSE, "i1"), unique_indices = scnst(FALSE, "i1"), update_computation = body)
    Condition
      Error in `infer_types_scatter()`:
      ! scatter requires at least one input and one update.

---

    Code
      infer_types_scatter(inputs = inputs, scatter_indices = scatter_indices,
        updates = updates, scatter_dimension_numbers = sdn, indices_are_sorted = scnst(
          FALSE, "i1"), unique_indices = scnst(FALSE, "i1"), update_computation = body)
    Condition
      Error in `infer_types_scatter()`:
      ! Number of inputs must equal number of updates.
      x Got 1 inputs and 2 updates.

---

    Code
      infer_types_scatter(inputs = inputs, scatter_indices = scatter_indices,
        updates = updates, scatter_dimension_numbers = sdn, indices_are_sorted = scnst(
          FALSE, "i1"), unique_indices = scnst(FALSE, "i1"), update_computation = body)
    Condition
      Error in `infer_types_scatter()`:
      ! rank of inputs must equal length(update_window_dims) + length(inserted_window_dims) + length(input_batching_dims).
      x Got rank = 3, but expected 2 (= 1 + 1 + 0).

---

    Code
      infer_types_scatter(inputs = inputs, scatter_indices = scatter_indices,
        updates = updates, scatter_dimension_numbers = sdn, indices_are_sorted = scnst(
          FALSE, "i1"), unique_indices = scnst(FALSE, "i1"), update_computation = body)
    Condition
      Error in `infer_types_scatter()`:
      ! `update_window_dims` must contain unique dimension indices
      x Got c(1, 1)

