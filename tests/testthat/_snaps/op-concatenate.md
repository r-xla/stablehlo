# errors

    Code
      infer_types_concatenate(dimension = scnst(0L, "i64"))
    Condition
      Error in `infer_types_concatenate()`:
      ! must have at least one input

---

    Code
      infer_types_concatenate(vt("f32", c(2L, 3L)), vt("i32", c(2L, 3L)), dimension = scnst(
        0L, "i64"))
    Condition
      Error in `infer_types_concatenate()`:
      ! Each input must have same data type
      x Got f32 and i32.

---

    Code
      infer_types_concatenate(vt("f32", c(2L, 3L)), dimension = scnst(2L, "i64"))
    Condition
      Error in `infer_types_concatenate()`:
      ! `dimension` contains index outside the valid range.
      x Got 2, but valid range is [0, 2).

---

    Code
      infer_types_concatenate(vt("f32", c(2L, 3L)), vt("f32", c(2L, 4L)), dimension = scnst(
        0L, "i64"))
    Condition
      Error in `infer_types_concatenate()`:
      ! All inputs must have the same shape, except in dimension(s) 0.
      x Got shapes 2x3 and 2x4.

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x1: tensor<3x1xi32>, %x2: tensor<3x2xi32>, %x3: tensor<3x3xi32>) -> tensor<3x6xi32> {\n%0 = \"stablehlo.concatenate\" (%x1, %x2, %x3) {\ndimension = 1 : i64\n}: (tensor<3x1xi32>, tensor<3x2xi32>, tensor<3x3xi32>) -> (tensor<3x6xi32>)\n\"func.return\"(%0): (tensor<3x6xi32>) -> ()\n}\n"

