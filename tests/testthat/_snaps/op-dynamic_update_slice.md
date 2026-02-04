# errors

    Code
      infer_types_dynamic_update_slice(vt("f32", c(4L, 5L)), vt("f32", c(2L, 3L)), vt(
        "i32", 2L), vt("i32", integer()))
    Condition
      Error in `infer_types_dynamic_update_slice()`:
      ! `start_indices[0]` must be 0-dimensional tensors.
      x Got shape (2).

---

    Code
      infer_types_dynamic_update_slice(vt("f32", c(4L, 5L)), vt("f32", c(2L, 3L, 1L)),
      vt("i32", integer()), vt("i32", integer()))
    Condition
      Error in `infer_types_dynamic_update_slice()`:
      ! rank(update) must equal rank(operand).
      x Got rank(update) = 3 and rank(operand) = 2.

---

    Code
      infer_types_dynamic_update_slice(vt("f32", c(4L, 5L)), vt("f32", c(2L, 3L)), vt(
        "i32", integer()))
    Condition
      Error in `infer_types_dynamic_update_slice()`:
      ! length(start_indices) must equal rank(operand).
      x Got 1 start_indices and rank 2.

---

    Code
      infer_types_dynamic_update_slice(vt("f32", c(4L, 5L)), vt("f32", c(5L, 3L)), vt(
        "i32", integer()), vt("i32", integer()))
    Condition
      Error in `infer_types_dynamic_update_slice()`:
      ! shape(update) must not be greater than shape(operand).
      x Got shape(update) (5x3) and shape(operand) (4x5).

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%operand: tensor<4x4xi32>, %update: tensor<2x2xi32>) -> tensor<4x4xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<-1> : tensor<i64>\n}: () -> (tensor<i64>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i64>\n}: () -> (tensor<i64>)\n%2 = \"stablehlo.dynamic_update_slice\" (%operand, %update, %0, %1): (tensor<4x4xi32>, tensor<2x2xi32>, tensor<i64>, tensor<i64>) -> (tensor<4x4xi32>)\n\"func.return\"(%2): (tensor<4x4xi32>) -> ()\n}\n"

