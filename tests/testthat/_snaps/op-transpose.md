# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3x4xi32>) -> tensor<4x2x3xi32> {\n%0 = \"stablehlo.transpose\" (%x) {\npermutation = array<i64: 2, 0, 1>\n}: (tensor<2x3x4xi32>) -> (tensor<4x2x3xi32>)\n\"func.return\"(%0): (tensor<4x2x3xi32>) -> ()\n}\n"

# errors

    Code
      infer_types_transpose(vt("f32", c(2L, 3L, 4L)), permutation = cnst(c(0L, 2L, 1L,
        3L), "i64", 4L))
    Condition
      Error in `infer_types_transpose()`:
      ! `permutation` must be a permutation of c(0, 1, 2).
      x Got c(0, 2, 1, 3).

