# assert_vt_has_ttype

    Code
      assert_vt_has_ttype(x = y, "BooleanType", shape = integer())
    Condition
      Error:
      ! `y` must have dtype BooleanType.
      x Got <i32>.

---

    Code
      assert_vt_has_ttype(x = y, "IntegerType", shape = 1L)
    Condition
      Error:
      ! `y` must have shape (1).
      x Got ().

---

    Code
      assert_vt_has_ttype(x = y, IntegerType(64L), shape = integer())
    Condition
      Error:
      ! `y` must have dtype i64.
      x Got <i32>.

# assert_vt_is_tensor

    Code
      assert_vt_is_tensor(x = 1)
    Condition
      Error:
      ! `1` must be a ValueType.
      x Got <numeric>.

---

    Code
      assert_vt_is_tensor(x = y)
    Condition
      Error:
      ! `y` must be a ValueType.
      x Got <integer>.

---

    Code
      assert_vt_is_tensor(x = token)
    Condition
      Error:
      ! `token` must contain a TensorType.
      x Got <TokenType>.

---

    Code
      assert_vt_is_tensor(x = z)

# assert_vts_are_tensors

    Code
      assert_vts_are_tensors(x, 1L)
    Condition
      Error in `assert_vts_are_tensors()`:
      ! `args[[i]]` must be a ValueType.
      x Got <integer>.

---

    Code
      assert_vts_are_tensors(x = token)
    Condition
      Error in `assert_vts_are_tensors()`:
      ! `x` must contain a TensorType.
      x Got <TokenType>.

# assert_vt_equal

    Code
      assert_vt_equal(x, z1)
    Condition
      Error:
      ! `x` and `z1` must have the same tensor type.
      x Got tensor<i32> and tensor<1xi32>.

---

    Code
      assert_vt_equal(x, z2)
    Condition
      Error:
      ! `x` and `z2` must have the same tensor type.
      x Got tensor<i32> and tensor<f32>.

# assert_vts_have_same_dtype

    Code
      assert_vts_have_same_dtype(x, y)
    Condition
      Error:
      ! `x` and `y` must have the same dtype.
      x Got <i32> and <f32>.

# assert_valid_id

    Code
      assert_valid_id("_foo")
    Condition
      Error:
      ! Identifiers can only contain {letters, digits, _}; They must start with a letter or be all digits.
      x `"_foo"` is "_foo".

---

    Code
      assert_valid_id("1abc")
    Condition
      Error:
      ! Identifiers can only contain {letters, digits, _}; They must start with a letter or be all digits.
      x `"1abc"` is "1abc".

---

    Code
      assert_valid_id("foo-bar")
    Condition
      Error:
      ! Identifiers can only contain {letters, digits, _}; They must start with a letter or be all digits.
      x `"foo-bar"` is "foo-bar".

---

    Code
      assert_valid_id("")
    Condition
      Error:
      ! Identifiers can only contain {letters, digits, _}; They must start with a letter or be all digits.
      x `""` is "".

# assert_one_of

    Code
      assert_one_of(x, c("TensorType", "TokenType"))
    Condition
      Error:
      ! `x` must be a <TensorType/TokenType>.
      x Got <ValueType>.

