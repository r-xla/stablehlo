# errors

    Code
      infer_types_gather(operand, start_indices, gather_dimension_numbers = gdn,
        slice_sizes = cnst(slice_sizes, "i64", length(slice_sizes)),
        indices_are_sorted = scnst(FALSE, "pred"))
    Condition
      Error in `infer_types_gather()`:
      ! rank(operand) must equal length(offset_dims) + length(collapsed_slice_dims) + length(operand_batching_dims).
      x Got rank = 3, but expected 2 (= 1 + 1 + 0).

---

    Code
      infer_types_gather(operand, start_indices, gather_dimension_numbers = gdn,
        slice_sizes = cnst(slice_sizes, "i64", length(slice_sizes)),
        indices_are_sorted = scnst(FALSE, "pred"))
    Condition
      Error in `infer_types_gather()`:
      ! `index_vector_dim` contains index outside the valid range.
      x Got 5, but valid range is [0, 3).

---

    Code
      infer_types_gather(operand, start_indices, gather_dimension_numbers = gdn,
        slice_sizes = cnst(slice_sizes, "i64", length(slice_sizes)),
        indices_are_sorted = scnst(FALSE, "pred"))
    Condition
      Error in `infer_types_gather()`:
      ! `offset_dims` must contain unique dimension indices
      x Got c(1, 1)

---

    Code
      infer_types_gather(operand, start_indices, gather_dimension_numbers = gdn,
        slice_sizes = cnst(slice_sizes, "i64", length(slice_sizes)),
        indices_are_sorted = scnst(FALSE, "pred"))
    Condition
      Error in `infer_types_gather()`:
      ! length(slice_sizes) must equal rank(operand).
      x Got 3, but expected 2.

# gather looks correct

    Code
      repr(func)
    Output
      [1] "func.func @main (%operand: tensor<3x4xi32>, %start_indices: tensor<2x2xi32>) -> tensor<2x2xi32> {\n%0 = \"stablehlo.gather\"(%operand, %start_indices) {\ndimension_numbers = #stablehlo.gather<offset_dims = [1], collapsed_slice_dims = [0], start_index_map = [0, 1], index_vector_dim = 1>,\nslice_sizes = array<i64: 1, 2>, indices_are_sorted = false\n}: (tensor<3x4xi32>, tensor<2x2xi32>) -> (tensor<2x2xi32>)\n\"func.return\"(%0): (tensor<2x2xi32>) -> ()\n}\n"

