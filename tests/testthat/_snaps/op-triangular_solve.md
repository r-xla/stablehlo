# errors

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! `a` must have rank >= 2
      x Got rank 1.

---

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! `a` and `b` must have the same rank
      x Got ranks 2 and 3.

---

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! `a` must be a square matrix (last two dimensions must be equal)
      x Got shape (3x4).

---

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! Batch dimensions of `a` and `b` must match
      x Got shapes (2) and (4).

---

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! Dimension mismatch
      x Got shapes (3x3) and (4x2).

---

    Code
      infer_types_triangular_solve(a, b, left_side = scnst(left_side, "pred"), lower = scnst(
        TRUE, "pred"), unit_diagonal = scnst(FALSE, "pred"), transpose_a = transpose_a)
    Condition
      Error in `infer_types_triangular_solve()`:
      ! `transpose_a` must be one of: "NO_TRANSPOSE", "TRANSPOSE", and "ADJOINT".
      x Got INVALID.

# basic triangular_solve

    Code
      repr(f)
    Output
      [1] "func.func @main (%a: tensor<3x3xf32>, %b: tensor<3x3xf32>) -> tensor<3x3xf32> {\n%0 = \"stablehlo.triangular_solve\" (%a, %b) {\nleft_side = true,\nlower = true,\nunit_diagonal = false,\ntranspose_a = #stablehlo<transpose NO_TRANSPOSE>\n}: (tensor<3x3xf32>, tensor<3x3xf32>) -> (tensor<3x3xf32>)\n\"func.return\"(%0): (tensor<3x3xf32>) -> ()\n}\n"

