# errors

    Code
      infer_types_if(pred, branch_i32, branch_2out)
    Condition
      Error in `infer_types_if()`:
      ! `true_branch` and `false_branch` must have the same number of outputs.
      i Got 1 and 2.

---

    Code
      infer_types_if(pred, branch_i32, branch_f32)
    Condition
      Error in `infer_types_if()`:
      ! `output_types(true_branch)[0]` and `output_types(false_branch)[0]` must have the same type.
      x Got tensor<2xi32> and tensor<2xf32>.

# If operator works

    Code
      f
    Output
      func.func @main (%pred: tensor<i1>, %x1: tensor<f32>, %x2: tensor<f32>) -> tensor<f32> {
      %0 = "stablehlo.if" (%pred)({
      "stablehlo.return"(%x1): (tensor<f32>) -> ()
      }, {
      "stablehlo.return"(%x2): (tensor<f32>) -> ()
      }): (tensor<i1>) -> (tensor<f32>)
      "func.return"(%0): (tensor<f32>) -> ()
      }

