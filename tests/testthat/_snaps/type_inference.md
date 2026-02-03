# infer_types_generic_biv errors

    Code
      infer_types_generic_biv(lhs, rhs)
    Condition
      Error in `infer_types_generic_biv()`:
      ! `lhs` must be a ValueType.
      x Got <character>.

---

    Code
      infer_types_generic_biv(lhs, rhs)
    Condition
      Error in `infer_types_generic_biv()`:
      ! `lhs` must contain a TensorType.
      x Got <TokenType>.

---

    Code
      infer_types_generic_biv(lhs, rhs)
    Condition
      Error in `infer_types_generic_biv()`:
      ! `lhs` and `rhs` must have the same tensor type.
      x Got tensor<2x3xi32> and tensor<2x3xf32>.

# infer_types_numeric_biv errors

    Code
      infer_types_numeric_biv(lhs, rhs)
    Condition
      Error in `infer_types_numeric_biv()`:
      ! `lhs` must have dtype FloatType, IntegerType, or UnsignedType.
      x Got i1.

---

    Code
      infer_types_numeric_biv(lhs, rhs)
    Condition
      Error in `infer_types_numeric_biv()`:
      ! `lhs` and `rhs` must have the same tensor type.
      x Got tensor<2x3xi32> and tensor<3x4xi32>.

# infer_types_float_biv errors

    Code
      infer_types_float_biv(lhs, rhs)
    Condition
      Error in `infer_types_float_biv()`:
      ! `lhs` must have dtype FloatType.
      x Got i32.

---

    Code
      infer_types_float_biv(lhs, rhs)
    Condition
      Error in `infer_types_float_biv()`:
      ! `lhs` and `rhs` must have the same tensor type.
      x Got tensor<2x3xf32> and tensor<3x4xf32>.

# infer_types_integerish_biv errors

    Code
      infer_types_integerish_biv(lhs, rhs)
    Condition
      Error in `infer_types_integerish_biv()`:
      ! `lhs` must have dtype BooleanType, IntegerType, or UnsignedType.
      x Got f32.

---

    Code
      infer_types_integerish_biv(lhs, rhs)
    Condition
      Error in `infer_types_integerish_biv()`:
      ! `rhs` must have dtype BooleanType, IntegerType, or UnsignedType.
      x Got f32.

---

    Code
      infer_types_integerish_biv(lhs, rhs)
    Condition
      Error in `infer_types_integerish_biv()`:
      ! `lhs` and `rhs` must have the same tensor type.
      x Got tensor<2x3xi32> and tensor<3x4xi32>.

# infer_types_generic_uni errors

    Code
      infer_types_generic_uni(operand)
    Condition
      Error in `infer_types_generic_uni()`:
      ! `operand` must be a ValueType.
      x Got <character>.

---

    Code
      infer_types_generic_uni(operand)
    Condition
      Error in `infer_types_generic_uni()`:
      ! `operand` must contain a TensorType.
      x Got <TokenType>.

# infer_types_numeric_uni errors

    Code
      infer_types_numeric_uni(operand)
    Condition
      Error in `infer_types_numeric_uni()`:
      ! `operand` must have dtype FloatType, IntegerType, or UnsignedType.
      x Got i1.

# infer_types_float_uni errors

    Code
      infer_types_float_uni(operand)
    Condition
      Error in `infer_types_float_uni()`:
      ! `operand` must have dtype FloatType.
      x Got i32.

# infer_types_integer_uni errors

    Code
      infer_types_integer_uni(operand)
    Condition
      Error in `infer_types_integer_uni()`:
      ! `operand` must have dtype IntegerType or UnsignedType.
      x Got f32.

# infer_types_integerish_uni errors

    Code
      infer_types_integerish_uni(operand)
    Condition
      Error in `infer_types_integerish_uni()`:
      ! `operand` must have dtype BooleanType, IntegerType, or UnsignedType.
      x Got f32.

