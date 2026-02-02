# matmul

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<5x4xf32>, %rhs: tensor<4x3xf32>) -> tensor<5x3xf32> {\n%0 = stablehlo.dot_general %lhs, %rhs, contracting_dims = [1] x [0]: (tensor<5x4xf32>, tensor<4x3xf32>) -> (tensor<5x3xf32>)\n\"func.return\"(%0): (tensor<5x3xf32>) -> ()\n}\n"

# batching_dims

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x5x4xf32>, %rhs: tensor<4x3x1xf32>) -> tensor<1x5x3xf32> {\n%0 = stablehlo.dot_general %lhs, %rhs, batching_dims = [0] x [2], contracting_dims = [2] x [0]: (tensor<1x5x4xf32>, tensor<4x3x1xf32>) -> (tensor<1x5x3xf32>)\n\"func.return\"(%0): (tensor<1x5x3xf32>) -> ()\n}\n"

# get nice error messages when shapes don't match

    Can't perform dot general where shape(lhs) = (10,1) and shape(rhs) = (5,3) because the sizes of the contracting_dims don't match.
    x They contracting_dims are 0 for lhs and 0 for rhs

# error messages

    `batching_dims` must have equal length for lhs and rhs.
    x Got lhs length 1 and rhs length 0.

---

    `contracting_dims` must have equal length for lhs and rhs.
    x Got lhs length 2 and rhs length 1.

---

    `lhs batching_dims and contracting_dims` contains duplicate dimension indices.
    x Got c(0, 0). Each dimension index must appear only once.

---

    `rhs batching_dims and contracting_dims` contains duplicate dimension indices.
    x Got c(0, 0). Each dimension index must appear only once.

---

    `lhs_batching_dims` contains index outside the valid range.
    x Got 5, but valid range is [0, 3).

---

    `lhs_contracting_dims` contains index outside the valid range.
    x Got 5, but valid range is [0, 2).

---

    `rhs_batching_dims` contains index outside the valid range.
    x Got 5, but valid range is [0, 2).

---

    `rhs_contracting_dims` contains index outside the valid range.
    x Got 5, but valid range is [0, 2).

---

    Can't perform dot general where shape(lhs) = (2,3) and shape(rhs) = (1,3) because the sizes of the batching_dims don't match.
    x They batching_dims are 0 for lhs and 0 for rhs

---

    Can't perform dot general where shape(lhs) = (2,3) and shape(rhs) = (1,4) because the sizes of the contracting_dims don't match.
    x They contracting_dims are 1 for lhs and 1 for rhs

