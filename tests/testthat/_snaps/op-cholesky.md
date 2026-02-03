# errors

    Code
      infer_types_cholesky(operand, lower = scnst(TRUE, "pred"))
    Condition
      Error in `infer_types_cholesky()`:
      ! `operand` needs to have at least rank = 2
      x Got rank = 1.

---

    Code
      infer_types_cholesky(operand, lower = scnst(TRUE, "pred"))
    Condition
      Error in `infer_types_cholesky()`:
      ! `operand` must be symmetric in the last two dimensions
      x Got shape (3,4).

# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xf64>) -> tensor<3x3xf64> {\n%0 = \"stablehlo.cholesky\" (%x) {\nlower = false\n}: (tensor<3x3xf64>) -> (tensor<3x3xf64>)\n\"func.return\"(%0): (tensor<3x3xf64>) -> ()\n}\n"

