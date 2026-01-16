# basic gather operation

    Code
      func
    Output
      func.func @main (%operand: tensor<3x4x2xf32>) -> tensor<2x2xf32> {
      %0 = "stablehlo.constant" () {
      value = dense<[[0, 0], [1, 2]]> : tensor<2x2xi64>
      }: () -> (tensor<2x2xi64>)
      %1 = "stablehlo.gather"(%operand, %0) {
      dimension_numbers = #stablehlo.gather<offset_dims = [1], collapsed_slice_dims = [0, 1], start_index_map = [0, 1], index_vector_dim = 1>,
      slice_sizes = array<i64: 1, 1, 2>, indices_are_sorted = false
      }: (tensor<3x4x2xf32>, tensor<2x2xi64>) -> (tensor<2x2xf32>)
      "func.return"(%1): (tensor<2x2xf32>) -> ()
      }

# gather with batch dimensions

    Code
      func
    Output
      func.func @main (%operand: tensor<2x3x4xf32>) -> tensor<2x3x4xf32> {
      %0 = "stablehlo.constant" () {
      value = dense<[[0, 2, 1], [1, 0, 2]]> : tensor<2x3xi64>
      }: () -> (tensor<2x3xi64>)
      %1 = "stablehlo.gather"(%operand, %0) {
      dimension_numbers = #stablehlo.gather<offset_dims = [2], collapsed_slice_dims = [1], operand_batching_dims = [0], start_indices_batching_dims = [0], start_index_map = [1], index_vector_dim = 2>,
      slice_sizes = array<i64: 1, 1, 4>, indices_are_sorted = false
      }: (tensor<2x3x4xf32>, tensor<2x3xi64>) -> (tensor<2x3x4xf32>)
      "func.return"(%1): (tensor<2x3x4xf32>) -> ()
      }

# simple gather: extract single elements

    Code
      func
    Output
      func.func @main (%operand: tensor<4x5xf32>) -> tensor<3xf32> {
      %0 = "stablehlo.constant" () {
      value = dense<[[0, 0], [1, 2], [2, 4]]> : tensor<3x2xi64>
      }: () -> (tensor<3x2xi64>)
      %1 = "stablehlo.gather"(%operand, %0) {
      dimension_numbers = #stablehlo.gather<offset_dims = [], collapsed_slice_dims = [0, 1], start_index_map = [0, 1], index_vector_dim = 1>,
      slice_sizes = array<i64: 1, 1>, indices_are_sorted = false
      }: (tensor<4x5xf32>, tensor<3x2xi64>) -> (tensor<3xf32>)
      "func.return"(%1): (tensor<3xf32>) -> ()
      }

