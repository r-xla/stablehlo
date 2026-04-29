# basic tests

    Code
      repr(fv)
    Output
      [1] "func.func @main (%x: tensor<2x5xf32>) -> tensor<2x3xf32> {\n%0, %1 = \"chlo.top_k\" (%x) {\nk = 3 : i64\n}: (tensor<2x5xf32>) -> (tensor<2x3xf32>, tensor<2x3xi32>)\nreturn %0 : tensor<2x3xf32>\n}\n"

---

    Code
      repr(fi)
    Output
      [1] "func.func @main (%x: tensor<2x5xf32>) -> tensor<2x3xi32> {\n%0, %1 = \"chlo.top_k\" (%x) {\nk = 3 : i64\n}: (tensor<2x5xf32>) -> (tensor<2x3xf32>, tensor<2x3xi32>)\nreturn %1 : tensor<2x3xi32>\n}\n"

# works on rank-1 input

    Code
      repr(fv1)
    Output
      [1] "func.func @main (%x: tensor<5xf32>) -> tensor<3xf32> {\n%0, %1 = \"chlo.top_k\" (%x) {\nk = 3 : i64\n}: (tensor<5xf32>) -> (tensor<3xf32>, tensor<3xi32>)\nreturn %0 : tensor<3xf32>\n}\n"

# errors

    Code
      infer_types_top_k(vt("f32", integer()), k = scnst(1L, "i64"))
    Condition
      Error in `infer_types_top_k()`:
      ! `operand` must have rank >= 1.
      x Got rank 0.

---

    Code
      infer_types_top_k(vt("f32", c(2L, 3L)), k = scnst(5L, "i64"))
    Condition
      Error in `infer_types_top_k()`:
      ! `k` must not exceed the size of the last dimension of `operand`.
      x Got k = 5 and last dimension size 3.

---

    Code
      infer_types_top_k(vt("f32", c(2L, 3L)), k = scnst(0L, "i64"))
    Condition
      Error in `infer_types_top_k()`:
      ! `k` must be a positive integer.
      x Got 0.

---

    Code
      infer_types_top_k(vt("pred", c(2L, 3L)), k = scnst(1L, "i64"))
    Condition
      Error in `infer_types_top_k()`:
      ! `operand` must have dtype FloatType, IntegerType, or UIntegerType.
      x Got bool.

