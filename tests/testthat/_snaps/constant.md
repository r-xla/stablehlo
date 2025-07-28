# Constant TensorConstant

    Code
      repr(const)
    Output
      [1] "dense<+1.0000000000000000e+00> : tensor<f32>"

# BooleanConstant works correctly

    Code
      repr(true_constant)
    Output
      [1] "true"

---

    Code
      repr(false_constant)
    Output
      [1] "false"

# IntegerConstant works correctly

    Code
      repr(int_constant)
    Output
      [1] "42"

---

    Code
      repr(negative_int)
    Output
      [1] "-123"

# FloatConstant works correctly

    Code
      repr(float_constant)
    Output
      [1] "+314.159E+0"

# TensorConstant works correctly

    Code
      repr(tensor_const)
    Output
      [1] "dense<+3.1400000000000001e+00> : tensor<f32>"

---

    Code
      repr(bool_tensor)
    Output
      [1] "dense<true> : tensor<i1>"

---

    Code
      repr(int_tensor)
    Output
      [1] "dense<42> : tensor<si64>"

