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
      Error in `if (nrow(pad) != rank || ncol(pad) != 2L) ...`:
      ! missing value where TRUE/FALSE needed

# basic reduce_window (sum pooling)

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<4x4xf32>) -> tensor<2x2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce_window\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n    \"stablehlo.return\"(%2): (tensor<f32>) -> ()\n}) {\nwindow_dimensions = array<i64: 2, 2>,\nwindow_strides = array<i64: 2, 2>,\nbase_dilations = array<i64: 1, 1>,\nwindow_dilations = array<i64: 1, 1>,\npadding = dense<[[0, 0], [0, 0]]> : tensor<2x2xi64>\n}: (tensor<4x4xf32>, tensor<f32>) -> (tensor<2x2xf32>)\n\"func.return\"(%1): (tensor<2x2xf32>) -> ()\n}\n"

