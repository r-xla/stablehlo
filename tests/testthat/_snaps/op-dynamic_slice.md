# errors

    Code
      infer_types_dynamic_slice(vt("f32", c(4L, 5L)), vt("i32", integer()),
      slice_sizes = cnst(c(2L, 3L), "i64", 2L))
    Condition
      Error in `infer_types_dynamic_slice()`:
      ! size(start_indices) must equal rank(operand).
      x Got 1 start_indices and rank 2.

---

    Code
      infer_types_dynamic_slice(vt("f32", c(4L, 5L)), vt("i32", integer()), vt("i32",
        integer()), slice_sizes = cnst(c(2L), "i64", 1L))
    Condition
      Error in `infer_types_dynamic_slice()`:
      ! size(slice_sizes) must equal rank(operand).
      x Got 1 slice_sizes and rank 2.

---

    Code
      infer_types_dynamic_slice(vt("f32", c(4L, 5L)), vt("i32", 2L), vt("i32",
        integer()), slice_sizes = cnst(c(2L, 3L), "i64", 2L))
    Condition
      Error in `infer_types_dynamic_slice()`:
      ! All `start_indices` must have the same type.
      x Got types: tensor<2xi32>, tensor<i32>.

---

    Code
      infer_types_dynamic_slice(vt("f32", c(4L, 5L)), vt("i32", integer()), vt("i32",
        integer()), slice_sizes = cnst(c(5L, 3L), "i64", 2L))
    Condition
      Error in `infer_types_dynamic_slice()`:
      ! `slice_sizes` must not be greater than `operand`'s shape.
      x Got slice_sizes (5,3) and operand shape (4,5).

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4x4xi32>) -> tensor<2x2xi32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<-1> : tensor<i64>\n}: () -> (tensor<i64>)\n%1 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i64>\n}: () -> (tensor<i64>)\n%2 = \"stablehlo.dynamic_slice\" (%x, %0, %1) {\nslice_sizes = array<i64: 2, 2>\n}: (tensor<4x4xi32>, tensor<i64>, tensor<i64>) -> (tensor<2x2xi32>)\n\"func.return\"(%2): (tensor<2x2xi32>) -> ()\n}\n"

