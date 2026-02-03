# errors

    Code
      infer_types_while(cond = cond_ok, body = body_ok)
    Condition
      Error in `infer_types_while()`:
      ! There must be at least one input

---

    Code
      infer_types_while(vt("i32", 2L), cond = cond_2in, body = body_ok)
    Condition
      Error in `infer_types_while()`:
      ! `cond` must have the same number of inputs as `...`
      x Got 2 and 1.

---

    Code
      infer_types_while(vt("i32", 2L), cond = cond_ok, body = body_wrong)
    Condition
      Error in `infer_types_while()`:
      ! `body output[0]` and `input[0]` must have the same type.
      x Got tensor<2xf32> and tensor<2xi32>.

# simple loop

    Code
      repr(func)
    Output
      [1] "func.func @main (%init_i: tensor<i64>, %init_sum: tensor<i64>) -> (tensor<i64>, tensor<i64>) {\n%0, %1 = \"stablehlo.while\" (%init_i, %init_sum)({\n  ^bb0(%arg0: tensor<i64>, %arg1: tensor<i64>):\n    %2 = \"stablehlo.constant\" () {\nvalue = dense<10> : tensor<i64>\n}: () -> (tensor<i64>)\n    %3 = stablehlo.compare LT, %arg0, %2, SIGNED : (tensor<i64>, tensor<i64>) -> (tensor<i1>)\n    \"stablehlo.return\"(%3): (tensor<i1>) -> ()\n}, {\n  ^bb0(%arg0: tensor<i64>, %arg1: tensor<i64>):\n    %4 = \"stablehlo.constant\" () {\nvalue = dense<1> : tensor<i64>\n}: () -> (tensor<i64>)\n    %5 = \"stablehlo.add\" (%arg1, %4): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    %6 = \"stablehlo.add\" (%arg0, %4): (tensor<i64>, tensor<i64>) -> (tensor<i64>)\n    \"stablehlo.return\"(%6, %5): (tensor<i64>, tensor<i64>) -> ()\n}): (tensor<i64>, tensor<i64>) -> (tensor<i64>, tensor<i64>)\n\"func.return\"(%0, %1): (tensor<i64>, tensor<i64>) -> ()\n}\n"

