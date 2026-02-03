# errors

    Code
      infer_types_select(vt("pred", c(3L, 3L)), vt("f32", c(2L, 3L)), vt("f32", c(2L,
        3L)))
    Condition
      Error in `infer_types_select()`:
      ! rank of `pred` must be 0 or equal to rank of `on_true`
      i Got shapes (3,3) and (2,3).

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%pred: tensor<2x3x2xi1>, %on_true: tensor<2x3x2xf32>, %on_false: tensor<2x3x2xf32>) -> tensor<2x3x2xf32> {\n%0 = \"stablehlo.select\" (%pred, %on_true, %on_false): (tensor<2x3x2xi1>, tensor<2x3x2xf32>, tensor<2x3x2xf32>) -> (tensor<2x3x2xf32>)\n\"func.return\"(%0): (tensor<2x3x2xf32>) -> ()\n}\n"

