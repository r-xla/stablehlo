# simple test

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<f32>, %rhs: tensor<f32>) -> tensor<i1> {\n%0 = stablehlo.compare LT, %lhs, %rhs, FLOAT : (tensor<f32>, tensor<f32>) -> (tensor<i1>)\n\"func.return\"(%0): (tensor<i1>) -> ()\n}\n"

# errors

    Code
      infer_types_compare(lhs, rhs, comparison_direction, compare_type)
    Condition
      Error in `infer_types_compare()`:
      ! `comparison_direction` must be one of "EQ", "NE", "GE", "GT", "LE", and "LT".
      x Got "INVALID".

---

    Code
      infer_types_compare(lhs, rhs, comparison_direction, compare_type)
    Condition
      Error in `infer_types_compare()`:
      ! `compare_type` must be one of "FLOAT", "TOTALORDER", "SIGNED", and "UNSIGNED".
      x Got "INVALID".

---

    Code
      infer_types_compare(lhs, rhs, comparison_direction, compare_type)
    Condition
      Error in `infer_types_compare()`:
      ! `compare_type` must be SIGNED for signed integer data types.

---

    Code
      infer_types_compare(lhs, rhs, comparison_direction, compare_type)
    Condition
      Error in `infer_types_compare()`:
      ! `compare_type` must be UNSIGNED for unsigned integer or boolean data types.

---

    Code
      infer_types_compare(lhs, rhs, comparison_direction, compare_type)
    Condition
      Error in `infer_types_compare()`:
      ! `compare_type` must be FLOAT or TOTALORDER for floating-point data types.

