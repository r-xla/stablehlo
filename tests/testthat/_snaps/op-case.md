# Case operator works

    Code
      f
    Output
      func.func @main (%index: tensor<i32>, %x1: tensor<2xi64>, %x2: tensor<2xi64>) -> tensor<2xi64> {
      %0 = "stablehlo.case" (%index)({
      "stablehlo.return"(%x1): (tensor<2xi64>) -> ()
      }, {
      "stablehlo.return"(%x2): (tensor<2xi64>) -> ()
      }): (tensor<i32>) -> (tensor<2xi64>)
      "func.return"(%0): (tensor<2xi64>) -> ()
      }

# errors

    Code
      infer_types_case(index)
    Condition
      Error in `infer_types_case()`:
      ! branches must be a non-empty list

---

    Code
      infer_types_case(index, "not a func")
    Condition
      Error in `get_branch_out_types()`:
      ! `branches[0]` must be a Func.
      x Got character.

---

    Code
      infer_types_case(index, branch_with_input)
    Condition
      Error:
      ! `branches[0]` must not have inputs.
      x Got 1 input.

---

    Code
      infer_types_case(index, branch_i32, branch_f32)
    Condition
      Error in `infer_types_case()`:
      ! All branch functions must have the same output types.
      x Got tensor<2xi32> and tensor<2xf32>.

