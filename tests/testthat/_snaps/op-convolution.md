# basic 2D convolution (NHWC, valid padding)

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x4x4x1xf32>, %rhs: tensor<3x3x1x1xf32>) -> tensor<1x2x2x1xf32> {\n%0 = \"stablehlo.convolution\"(%lhs, %rhs) {\nwindow_strides = array<i64: 1, 1>,\npadding = dense<[[0, 0], [0, 0]]> : tensor<2x2xi64>,\nlhs_dilation = array<i64: 1, 1>,\nrhs_dilation = array<i64: 1, 1>,\nwindow_reversal = array<i1: false, false>,\nfeature_group_count = 1 : i64,\nbatch_group_count = 1 : i64,\ndimension_numbers = #stablehlo.conv<[b, 0, 1, f]x[0, 1, i, o]->[b, 0, 1, f]>,\nprecision_config = [#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]\n}: (tensor<1x4x4x1xf32>, tensor<3x3x1x1xf32>) -> (tensor<1x2x2x1xf32>)\nreturn %0 : tensor<1x2x2x1xf32>\n}\n"

# 2D convolution with strides and padding

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x5x5x1xf32>, %rhs: tensor<3x3x1x1xf32>) -> tensor<1x3x3x1xf32> {\n%0 = \"stablehlo.convolution\"(%lhs, %rhs) {\nwindow_strides = array<i64: 2, 2>,\npadding = dense<[[1, 1], [1, 1]]> : tensor<2x2xi64>,\nlhs_dilation = array<i64: 1, 1>,\nrhs_dilation = array<i64: 1, 1>,\nwindow_reversal = array<i1: false, false>,\nfeature_group_count = 1 : i64,\nbatch_group_count = 1 : i64,\ndimension_numbers = #stablehlo.conv<[b, 0, 1, f]x[0, 1, i, o]->[b, 0, 1, f]>,\nprecision_config = [#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]\n}: (tensor<1x5x5x1xf32>, tensor<3x3x1x1xf32>) -> (tensor<1x3x3x1xf32>)\nreturn %0 : tensor<1x3x3x1xf32>\n}\n"

# convolution with feature_group_count (depthwise)

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x4x4x4xf32>, %rhs: tensor<3x3x1x4xf32>) -> tensor<1x2x2x4xf32> {\n%0 = \"stablehlo.convolution\"(%lhs, %rhs) {\nwindow_strides = array<i64: 1, 1>,\npadding = dense<[[0, 0], [0, 0]]> : tensor<2x2xi64>,\nlhs_dilation = array<i64: 1, 1>,\nrhs_dilation = array<i64: 1, 1>,\nwindow_reversal = array<i1: false, false>,\nfeature_group_count = 4 : i64,\nbatch_group_count = 1 : i64,\ndimension_numbers = #stablehlo.conv<[b, 0, 1, f]x[0, 1, i, o]->[b, 0, 1, f]>,\nprecision_config = [#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]\n}: (tensor<1x4x4x4xf32>, tensor<3x3x1x4xf32>) -> (tensor<1x2x2x4xf32>)\nreturn %0 : tensor<1x2x2x4xf32>\n}\n"

# convolution with non-default precision_config

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x4x4x1xf32>, %rhs: tensor<3x3x1x1xf32>) -> tensor<1x2x2x1xf32> {\n%0 = \"stablehlo.convolution\"(%lhs, %rhs) {\nwindow_strides = array<i64: 1, 1>,\npadding = dense<[[0, 0], [0, 0]]> : tensor<2x2xi64>,\nlhs_dilation = array<i64: 1, 1>,\nrhs_dilation = array<i64: 1, 1>,\nwindow_reversal = array<i1: false, false>,\nfeature_group_count = 1 : i64,\nbatch_group_count = 1 : i64,\ndimension_numbers = #stablehlo.conv<[b, 0, 1, f]x[0, 1, i, o]->[b, 0, 1, f]>,\nprecision_config = [#stablehlo<precision HIGH>, #stablehlo<precision HIGHEST>]\n}: (tensor<1x4x4x1xf32>, tensor<3x3x1x1xf32>) -> (tensor<1x2x2x1xf32>)\nreturn %0 : tensor<1x2x2x1xf32>\n}\n"

# rank-3 (1D) convolution

    Code
      repr(f)
    Output
      [1] "func.func @main (%lhs: tensor<1x8x1xf32>, %rhs: tensor<3x1x1xf32>) -> tensor<1x6x1xf32> {\n%0 = \"stablehlo.convolution\"(%lhs, %rhs) {\nwindow_strides = array<i64: 1>,\npadding = dense<[[0, 0]]> : tensor<1x2xi64>,\nlhs_dilation = array<i64: 1>,\nrhs_dilation = array<i64: 1>,\nwindow_reversal = array<i1: false>,\nfeature_group_count = 1 : i64,\nbatch_group_count = 1 : i64,\ndimension_numbers = #stablehlo.conv<[b, 0, f]x[0, i, o]->[b, 0, f]>,\nprecision_config = [#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]\n}: (tensor<1x8x1xf32>, tensor<3x1x1xf32>) -> (tensor<1x6x1xf32>)\nreturn %0 : tensor<1x6x1xf32>\n}\n"

# ConvDimensionNumbers repr

    Code
      cat(repr(dn))
    Output
      #stablehlo.conv<[b, 0, 1, f]x[0, 1, i, o]->[b, 0, 1, f]>

# PrecisionConfig repr

    Code
      cat(repr(pc))
    Output
      [#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]

---

    Code
      cat(repr(pc2))
    Output
      [#stablehlo<precision HIGH>, #stablehlo<precision HIGHEST>]

# error messages

    `lhs` and `rhs` must have the same rank.
    x Got ranks 4 and 3.

---

    `window_strides` must have length N - 2.
    x Expected length 2, got 3.

---

    `window_strides` must be positive.
    x Got c(0, 1).

---

    `padding` must have shape [N - 2, 2].
    x Expected shape [2, 2], got [1, 2].

---

    `lhs_dilation` must be positive.
    x Got c(0, 1).

---

    `rhs_dilation` must be positive.
    x Got c(1, 0).

---

    dim(lhs, input_feature_dimension) must be divisible by `feature_group_count`.
    x Got dim = 3, feature_group_count = 2.

---

    dim(rhs, kernel_input_feature_dimension) must equal dim(lhs, input_feature_dimension) / feature_group_count.
    x Got dim = 1, expected 4.

---

    `feature_group_count` must be positive.
    x Got 0.

---

    `batch_group_count` must be positive.
    x Got 0.

---

    At least one of `feature_group_count` or `batch_group_count` must be 1.
    x Got feature_group_count = 2 and batch_group_count = 2.

---

    `precision_config` must have length 2.
    x Got length 1.

---

    `precision_config` entries must be one of "DEFAULT", "HIGH", and "HIGHEST".
    x Got "DEFAULT" and "FOO".

# error: dimension uniqueness

    `input_batch_dimension/input_spatial_dimensions/input_feature_dimension` must contain unique dimension indices
    x Got c(0, 1, 2, 0)

# error: dimension out of bounds

    `input_batch_dimension/input_spatial_dimensions/input_feature_dimension` contains index outside the valid range.
    x Got c(0, 1, 2, 5), but valid range is [0, 4).

# error: lhs and rhs different dtypes

    `lhs` and `rhs` must have the same element type.
    x Got f32 and f64.

