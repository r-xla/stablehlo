# assert_vt_is_tensor

    Code
      assert_vt_is_tensor(x = 1)
    Condition
      Error:
      ! Expected `1` to have class stablehlo::ValueType.
      i Got <numeric>.

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(BooleanType), expected_shape = integer())
    Condition
      Error in `assert_vt_is_tensor()`:
      ! Expected `y` to have dtype BooleanType.
      i Got <IntegerType(32)>.

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType), expected_shape = 1L)
    Condition
      Error in `assert_vt_is_tensor()`:
      ! `y` must have shape (1).
      i Got shape ().

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType), expected_shape = integer())

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType(32)))

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(IntegerType(64)))
    Condition
      Error in `assert_vt_is_tensor()`:
      ! Expected `y` to have dtype IntegerType(64).
      i Got <IntegerType(32)>.

---

    Code
      assert_vt_is_tensor(x = y, expected_dtypes = list(BooleanType, IntegerType(32)))

---

    Code
      assert_vt_is_tensor(x = y)
    Condition
      Error:
      ! Expected `y` to have class stablehlo::ValueType.
      i Got <integer>.

---

    Code
      assert_vt_is_tensor(x = token)
    Condition
      Error:
      ! Expected `token@value` to have class stablehlo::TensorType.
      i Got <TokenType>.

---

    Code
      assert_vt_is_tensor(x = z)

# assert_vts_are_tensors

    Code
      assert_vts_are_tensors(x, 1L)
    Condition
      Error in `assert_vts_are_tensors()`:
      ! Expected `1L` to have class stablehlo::ValueType.
      i Got <integer>.

---

    Code
      assert_vts_are_tensors(x = token)
    Condition
      Error in `assert_vts_are_tensors()`:
      ! Expected `x@value` to have class stablehlo::TensorType.
      i Got <TokenType>.

