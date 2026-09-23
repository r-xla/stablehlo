# basic reduce_window (sum pooling)

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<4x4xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce_window\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = stablehlo.add %a, %b : tensor<f32>\n    stablehlo.return %2 : tensor<f32>\n}) {\nwindow_dimensions = array<i64: 2, 2>,\nwindow_strides = array<i64: 2, 2>,\nbase_dilations = array<i64: 1, 1>,\nwindow_dilations = array<i64: 1, 1>,\npadding = dense<[[0, 0], [0, 0]]> : tensor<2x2xi64>\n}: (tensor<4x4xf32>, tensor<f32>) -> (tensor<2x2xf32>)\nreturn %1 : tensor<2x2xf32>\n}\n"

# errors

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), body = body, window_dimensions = cnst(
        c(2L, 2L), "i64", 2L), window_strides = s2, base_dilations = d2,
      window_dilations = d2, padding = pad2)
    Condition
      Error in `infer_types_reduce_window()`:
      ! Number of arguments must be divisible by 2 (pairs of inputs and init values).
      x Got 1 argument.

---

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(2L, 2L, 2L), "i64", 3L), window_strides = s2,
      base_dilations = d2, window_dilations = d2, padding = pad2)
    Condition
      Error in `infer_types_reduce_window()`:
      ! `window_dimensions` must have length equal to input rank.
      x Expected length 2, got 3.

---

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(0L, 2L), "i64", 2L), window_strides = s2,
      base_dilations = d2, window_dilations = d2, padding = pad2)
    Condition
      Error in `infer_types_reduce_window()`:
      ! `window_dimensions` must be positive.
      x Got (0x2)

---

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L), window_strides = cnst(c(0L, 1L),
      "i64", 2L), base_dilations = d2, window_dilations = d2, padding = pad2)
    Condition
      Error in `infer_types_reduce_window()`:
      ! `window_strides` must be positive.
      x Got (0x1)

---

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L), window_strides = s2,
      base_dilations = d2, window_dilations = d2, padding = cnst(c(0L, 0L), "i64", c(
        1L, 2L)))
    Condition
      Error in `infer_types_reduce_window()`:
      ! `padding` must have shape [rank, 2].
      x Expected shape (2x2), got (1x2).

# the reducer accumulates into a promoted element type

    Code
      rw("f64", "f32")
    Condition
      Error in `infer_types_reduce_window()`:
      ! `body` must reduce into a type its input promotes to.
      x Input 0 has type f64, which does not promote to f32.

# a zero window dilation is rejected

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L), window_strides = cnst(c(1L, 1L),
      "i64", 2L), base_dilations = cnst(c(1L, 1L), "i64", 2L), window_dilations = cnst(
        c(0L, 1L), "i64", 2L), padding = cnst(c(0L, 0L, 0L, 0L), "i64", c(2L, 2L)))
    Condition
      Error in `infer_types_reduce_window()`:
      ! `window_dilations` must be positive.
      x Got (0x1)

# a padding that empties a dimension past zero is rejected

    Code
      infer_types_reduce_window(vt("f32", c(4L, 4L)), vt("f32", integer()), body = body,
      window_dimensions = cnst(c(2L, 2L), "i64", 2L), window_strides = cnst(c(1L, 1L),
      "i64", 2L), base_dilations = cnst(c(1L, 1L), "i64", 2L), window_dilations = cnst(
        c(1L, 1L), "i64", 2L), padding = cnst(c(-100L, -100L, -100L, -100L), "i64", c(
        2L, 2L)))
    Condition
      Error in `infer_types_reduce_window()`:
      ! `padding` must not remove more than a dimension of `inputs` holds.
      x Dimensions c(0, 1) dilate to c(4, 4), and padding c(-100, -100) and c(-100, -100) leaves c(-196, -196).

# a dilation or padding that overflows the window is refused

    Code
      rw(c(2000000000L, 1L), c(0L, 0L, 0L, 0L))
    Condition
      Error in `infer_types_reduce_window()`:
      ! The reduced window's result must have at most 2147483647 elements in each dimension.
      x Dimension 0 would be 6e+09.

---

    Code
      rw(c(1L, 1L), rep(2000000000L, 4L))
    Condition
      Error in `infer_types_reduce_window()`:
      ! The reduced window's result must have at most 2147483647 elements in each dimension.
      x Dimensions c(0, 1) would be c(4000000003, 4000000003).

