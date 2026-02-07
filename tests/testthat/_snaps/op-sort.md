# basic tests

    Code
      lapply(f, repr)
    Output
      [[1]]
      [1] "func.func @main (%x1: tensor<2x3xi32>, %x2: tensor<2x3xi32>) -> tensor<2x3xi32> {\n%0, %1 = \"stablehlo.sort\" (%x1, %x2)({\n  ^bb0(%arg1: tensor<i32>, %arg2: tensor<i32>, %arg3: tensor<i32>, %arg4: tensor<i32>):\n    %2 = stablehlo.compare GT, %arg1, %arg2, SIGNED : (tensor<i32>, tensor<i32>) -> (tensor<i1>)\n    \"stablehlo.return\"(%2): (tensor<i1>) -> ()\n}) {\ndimension = 0 : i64,\nis_stable = true\n}: (tensor<2x3xi32>, tensor<2x3xi32>) -> (tensor<2x3xi32>, tensor<2x3xi32>)\n\"func.return\"(%0): (tensor<2x3xi32>) -> ()\n}\n"
      
      [[2]]
      [1] "func.func @main (%x1: tensor<2x3xi32>, %x2: tensor<2x3xi32>) -> tensor<2x3xi32> {\n%0, %1 = \"stablehlo.sort\" (%x1, %x2)({\n  ^bb0(%arg1: tensor<i32>, %arg2: tensor<i32>, %arg3: tensor<i32>, %arg4: tensor<i32>):\n    %2 = stablehlo.compare GT, %arg1, %arg2, SIGNED : (tensor<i32>, tensor<i32>) -> (tensor<i1>)\n    \"stablehlo.return\"(%2): (tensor<i1>) -> ()\n}) {\ndimension = 0 : i64,\nis_stable = true\n}: (tensor<2x3xi32>, tensor<2x3xi32>) -> (tensor<2x3xi32>, tensor<2x3xi32>)\n\"func.return\"(%1): (tensor<2x3xi32>) -> ()\n}\n"
      

# errors

    Code
      infer_types_sort(dimension = scnst(0L, "i64"), is_stable = scnst(TRUE, "pred"),
      comparator = comparator)
    Condition
      Error in `infer_types_sort()`:
      ! provide at least one input

---

    Code
      infer_types_sort(vt("i32", c(2L, 3L)), vt("i32", c(3L, 3L)), dimension = scnst(
        0L, "i64"), is_stable = scnst(TRUE, "pred"), comparator = comparator)
    Condition
      Error in `infer_types_sort()`:
      ! Each input must have the same shape
      x Got shapes (2x3) and (3x3).

---

    Code
      infer_types_sort(vt("i32", c(2L, 3L)), dimension = scnst(5L, "i64"), is_stable = scnst(
        TRUE, "pred"), comparator = comparator)
    Condition
      Error in `infer_types_sort()`:
      ! `dimension` contains index outside the valid range.
      x Got 5, but valid range is [-2, 2).

