# errors

    Code
      infer_types_reverse(vt("f32", c(2L, 3L)), dimensions = cnst(c(0L, 0L), "i64",
      2L))
    Condition
      Error in `infer_types_reverse()`:
      ! `dimensions` must contain unique dimension indices
      x Got c(0, 0)

---

    Code
      infer_types_reverse(vt("f32", c(2L, 3L)), dimensions = cnst(integer(), "i64",
      0L))
    Condition
      Error in `infer_types_reverse()`:
      ! at least one dimension needs to be provided

---

    Code
      infer_types_reverse(vt("f32", c(2L, 3L)), dimensions = cnst(5L, "i64", 1L))
    Condition
      Error in `infer_types_reverse()`:
      ! `dimensions` contains index outside the valid range.
      x Got 5, but valid range is [0, 2).

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x2xf32>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.reverse\" (%x) {\ndimensions = array<i64: 1, 0>\n}: (tensor<2x3x2xf32>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%0): (tensor<2x3x2xf32>) -> ()\n}\n"

