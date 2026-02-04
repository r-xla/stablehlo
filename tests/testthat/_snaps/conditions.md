# ErrorIndexOutOfBounds

    `dimension` contains index outside the valid range.
    x Got 5, but valid range is [0, 3).

---

    `dimensions` contains index outside the valid range.
    x Got c(0, 1, 5), but valid range is [0, 3).

---

    `alias_indices` contains index outside the valid range.
    x Got 7, but valid range is [0, 5).

# ErrorDimensionUniqueness

    `dimensions` must contain unique dimension indices
    x Got c(0, 1, 0, 2)

# ErrorPermuteIndex

    `permutation` must be a permutation of c(0, 1, 2).
    x Got c(0, 2, 1, 3).

# ErrorUnequalTypes

    `output_types(true_branch)[2]` and `output_types(false_branch)[2]` must have the same type.
    x Got tensor<2x2xf32> and tensor<2x2xi32>.

# ErrorUnexpectedListType

    `init_values[0]` must be 0-D tensors.
    x Got tensor<2x2xf32>.

# ErrorIndicesNotSorted

    `update_window_dims` must be sorted in ascending order.
    x Got c(2, 0, 1).

# ErrorIndexInSet

    `index_vector_dim` must not be in `scatter_indices_batching_dims`.
    x index_vector_dim = 2 is in scatter_indices_batching_dims = c(0, 1, 2).

# cli_format.TensorDataType uses repr()

    Got f32

# cli_format.Shape uses repr()

    Got 2x3

# cli_format.ValueType uses repr()

    Got tensor<2x3xf32>

