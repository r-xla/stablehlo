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

# basic edge padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<4x5xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 1>,\nedge_padding_high = array<i64: 2, 1>,\ninterior_padding = array<i64: 0, 0>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<4x5xi32>)\n\"func.return\"(%1): (tensor<4x5xi32>) -> ()\n}\n"

# interior padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xi32>) -> tensor<3x7xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0> : tensor<i32>\n}: () -> (tensor<i32>)\n%1 = \"stablehlo.pad\" (%x, %0) {\nedge_padding_low = array<i64: 0, 0>,\nedge_padding_high = array<i64: 0, 0>,\ninterior_padding = array<i64: 1, 2>\n}: (tensor<2x3xi32>, tensor<i32>) -> (tensor<3x7xi32>)\n\"func.return\"(%1): (tensor<3x7xi32>) -> ()\n}\n"

