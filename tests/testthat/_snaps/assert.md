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

