# basic edge padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<4x5xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 1>,\nedge_padding_high = array<i64: 2, 1>,\ninterior_padding = array<i64: 0, 0>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<4x5xi32>)\nreturn %1 : tensor<4x5xi32>\n}\n"

# interior padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<3x7xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 0>,\nedge_padding_high = array<i64: 0, 0>,\ninterior_padding = array<i64: 1, 2>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<3x7xi32>)\nreturn %1 : tensor<3x7xi32>\n}\n"

# errors

    Code
      infer_types_pad(operand, padding_value, edge_padding_low = cnst(low, "i64",
        length(low)), edge_padding_high = cnst(high, "i64", length(high)),
      interior_padding = cnst(interior, "i64", length(interior)))
    Condition
      Error in `infer_types_pad()`:
      ! interior_padding must be non-negative
      x interior_padding: c(-1, 0)

---

    Code
      infer_types_pad(operand, padding_value, edge_padding_low = cnst(low, "i64",
        length(low)), edge_padding_high = cnst(high, "i64", length(high)),
      interior_padding = cnst(interior, "i64", length(interior)))
    Condition
      Error in `check()`:
      ! edge_padding_low must have length equal to operand rank
      x length(edge_padding_low): 1, operand_rank: 2

---

    Code
      infer_types_pad(operand, padding_value, edge_padding_low = cnst(low, "i64",
        length(low)), edge_padding_high = cnst(high, "i64", length(high)),
      interior_padding = cnst(interior, "i64", length(interior)))
    Condition
      Error in `infer_types_pad()`:
      ! `padding_value` must be a 0-dimensional tensor.
      x Got shape (2).

---

    Code
      infer_types_pad(operand, padding_value, edge_padding_low = cnst(low, "i64",
        length(low)), edge_padding_high = cnst(high, "i64", length(high)),
      interior_padding = cnst(interior, "i64", length(interior)))
    Condition
      Error in `infer_types_pad()`:
      ! `edge_padding_low` and `edge_padding_high` must not remove more elements than a dimension holds.
      x Padding `operand` of shape (3) by -3 and -3 would give -3.

---

    Code
      infer_types_pad(operand, padding_value, edge_padding_low = cnst(low, "i64",
        length(low)), edge_padding_high = cnst(high, "i64", length(high)),
      interior_padding = cnst(interior, "i64", length(interior)))
    Condition
      Error in `infer_types_pad()`:
      ! `edge_padding_low` and `edge_padding_high` must not remove more elements than a dimension holds.
      x Padding `operand` of shape (1x5x5) by c(-2, 0, 0) and c(0, 0, 0) would give c(-1, 5, 5).

# a padding that overflows the result is refused, not turned into an NA

    Code
      hlo_pad(x, pad_val, 2000000000L, 2000000000L, 0L)
    Condition
      Error:
      ! The padded tensor must have at most 2147483647 elements in each dimension.
      x Dimension 0 would be 4000000004.

---

    Code
      hlo_pad(x, pad_val, 0L, 0L, 2000000000L)
    Condition
      Error:
      ! The padded tensor must have at most 2147483647 elements in each dimension.
      x Dimension 0 would be 6000000004.

