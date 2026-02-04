# errors

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! `start_indices` must have same length as `limit_indices`
      x Got lengths 1 and 2.

---

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! length of `start_indices`, `limit_indices` and `strides` must be equal to operand's rank (2).
      x Got length 3.

---

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! `start_indices` contains index outside the valid range.
      x Got -1, but valid range is [0, 4).

---

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! `start_indices` contains index outside the valid range.
      x Got 3, but valid range is [0, 2).

---

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! `limit_indices` contains index outside the valid range.
      x Got 6, but valid range is [0, 5).

---

    Code
      infer_types_slice(operand, start_indices = cnst(start, "i64", length(start)),
      limit_indices = cnst(limit, "i64", length(limit)), strides = cnst(strides,
        "i64", length(strides)))
    Condition
      Error in `infer_types_slice()`:
      ! `strides` must be non-negative

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4x3xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.slice\" (%x) {\nstart_indices = array<i64: 2, 1>,\nlimit_indices = array<i64: 4, 3>,\nstrides = array<i64: 1, 1>\n}: (tensor<4x3xf32>) -> (tensor<2x2xf32>)\n\"func.return\"(%0): (tensor<2x2xf32>) -> ()\n}\n"

