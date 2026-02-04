# errors

    Code
      infer_types_reduce(inputs = list(), init_values = list(), body = body,
      dimensions = cnst(0L, "i64", 1L))
    Condition
      Error in `infer_types_reduce()`:
      ! No inputs provided

---

    Code
      infer_types_reduce(inputs = list(vt("f32", c(2L, 3L))), init_values = list(vt(
        "f32", integer()), vt("f32", integer())), body = body, dimensions = cnst(0L,
        "i64", 1L))
    Condition
      Error in `infer_types_reduce()`:
      ! Number of inputs must equal number of `init_values`
      x Got 1 inputs and 2 init_values.

---

    Code
      infer_types_reduce(inputs = list(vt("f32", c(2L, 3L))), init_values = list(vt(
        "f32", 2L)), body = body, dimensions = cnst(0L, "i64", 1L))
    Condition
      Error in `FUN()`:
      ! `init_values` must be 0-D tensors

---

    Code
      infer_types_reduce(inputs = list(vt("f32", c(2L, 3L))), init_values = list(vt(
        "f32", integer())), body = body, dimensions = cnst(5L, "i64", 1L))
    Condition
      Error in `infer_types_reduce()`:
      ! `dimensions` contains index outside the valid range.
      x Got 5, but valid range is [0, 2).

---

    Code
      infer_types_reduce(inputs = list(vt("f32", c(2L, 3L))), init_values = list(vt(
        "f32", integer())), body = body, dimensions = cnst(c(0L, 0L), "i64", 2L))
    Condition
      Error in `infer_types_reduce()`:
      ! `dimensions` must contain unique dimension indices
      x Got c(0, 0)

# basic tests

    Code
      func
    Output
      func.func @main (%x: tensor<2x3xf32>) -> tensor<2xf32> {
      %0 = "stablehlo.constant" () {
      value = dense<0.00000000e+00> : tensor<f32>
      }: () -> (tensor<f32>)
      %1 = "stablehlo.reduce" (%x, %0)({
        ^bb0(%a: tensor<f32>, %b: tensor<f32>):
          %2 = "stablehlo.add" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
          "stablehlo.return"(%2): (tensor<f32>) -> ()
      }) {
      dimensions = array<i64: 1>
      }: (tensor<2x3xf32>, tensor<f32>) -> (tensor<2xf32>)
      "func.return"(%1): (tensor<2xf32>) -> ()
      }

---

    Code
      repr(func)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2xf32> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<0.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n%1 = \"stablehlo.reduce\" (%x, %0)({\n  ^bb0(%a: tensor<f32>, %b: tensor<f32>):\n    %2 = \"stablehlo.add\" (%a, %b): (tensor<f32>, tensor<f32>) -> (tensor<f32>)\n    \"stablehlo.return\"(%2): (tensor<f32>) -> ()\n}) {\ndimensions = array<i64: 1>\n}: (tensor<2x3xf32>, tensor<f32>) -> (tensor<2xf32>)\n\"func.return\"(%1): (tensor<2xf32>) -> ()\n}\n"

