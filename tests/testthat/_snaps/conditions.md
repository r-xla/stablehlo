# InvalidIdentifierError

    Identifiers must start with a letter and contain only letters, numbers, and underscores.
    i Got "_name".

# UnequalTensorTypesError

    Expected all arguments to have the same tensor type.
    i Got x=tensor<2x3xi32>, y=tensor<2x3xf32>.

# ClassError

    Expected `x` to have class TensorType or TokenType.
    i Got <ValueType>.

# TensorDTypeError

    Expected `operand` to have dtype i32 or i64.
    i Got <f32>.

# TensorNDimsError

    `operand` must have at least 0 dimensions.
    i Got -1 dimensions.

---

    `operand` must have less than 1 dimensions.
    i Got 2 dimensions.

---

    `operand` must have between 1 and 2 dimensions (inclusive).
    i Got 0 dimensions.

---

    `operand` must have exactly 2 dimensions.
    i Got 1 dimension.

# TensorShapeError

    Expected `operand` to have shape (2,3).
    i Got (2,4).

# ShapeMismatchError

    Dimension 0 of `lhs` must match dimension 1 of `rhs`.
    i Got `lhs`[0] = 10 and `rhs`[1] = 20.

# DimensionOutOfRangeError

    `dimension` contains invalid dimension index.
    i Got dimension index 5, but valid range is [0, 3).

---

    `dimensions` contains invalid dimension index.
    i Got dimension indices: 0, 1, 5, but valid range is [0, 3).

# DimensionUniquenessError

    `dimensions` contains duplicate dimension indices.
    i Got [0, 1, 0, 2]. Each dimension index must appear only once.

# IndexOutOfBoundsError

    `alias_indices` contains index outside the valid range.
    i Valid range is [0, 5).

# SliceIndexError

    `start_indices` contains invalid start indices.
    i Got indices: -1, 0.

---

    `limit_indices` contains invalid limit indices.
    i Got indices: 10, 20.

# PermutationError

    `permutation` must be a permutation of [0, 1, ..., 2].
    i Got [0, 2, 1, 3], but expected a permutation of [0, 1, 2].

