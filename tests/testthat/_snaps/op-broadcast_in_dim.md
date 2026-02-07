# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<1x3xf32>) -> tensor<2x1x3xf32> {\n%0 = \"stablehlo.broadcast_in_dim\" (%x) {\nbroadcast_dimensions = array<i64: 0, 2>\n}: (tensor<1x3xf32>) -> (tensor<2x1x3xf32>)\n\"func.return\"(%0): (tensor<2x1x3xf32>) -> ()\n}\n"

# errors

    Code
      infer_types_broadcast_in_dim(operand, broadcast_dimensions = cnst(
        broadcast_dimensions, "i64", length(broadcast_dimensions)), shape = shape)
    Condition
      Error in `infer_types_broadcast_in_dim()`:
      ! Length of `broadcast_dimensions` must equal rank of `operand`.
      x Got 1 broadcast_dimensions for operand of rank 2.

---

    Code
      infer_types_broadcast_in_dim(operand, broadcast_dimensions = cnst(
        broadcast_dimensions, "i64", length(broadcast_dimensions)), shape = shape)
    Condition
      Error in `infer_types_broadcast_in_dim()`:
      ! `broadcast_dimensions` contains index outside the valid range.
      x Got c(0, 5), but valid range is [0, 3).

---

    Code
      infer_types_broadcast_in_dim(operand, broadcast_dimensions = cnst(
        broadcast_dimensions, "i64", length(broadcast_dimensions)), shape = shape)
    Condition
      Error in `infer_types_broadcast_in_dim()`:
      ! `broadcast_dimensions` must contain unique dimension indices
      x Got c(0, 0)

---

    Code
      infer_types_broadcast_in_dim(operand, broadcast_dimensions = cnst(
        broadcast_dimensions, "i64", length(broadcast_dimensions)), shape = shape)
    Condition
      Error in `infer_types_broadcast_in_dim()`:
      ! `operand` dimension 0 and `result` dimension 0 must match unless `operand` dim is 1.
      x Got shapes (2x3) and (4x5).

