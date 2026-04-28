make_dn_nhwc <- function(n_spatial = 2L) {
  ConvDimensionNumbers(
    input_batch_dimension = 0L,
    input_feature_dimension = n_spatial + 1L,
    input_spatial_dimensions = seq_len(n_spatial),
    kernel_input_feature_dimension = n_spatial,
    kernel_output_feature_dimension = n_spatial + 1L,
    kernel_spatial_dimensions = seq_len(n_spatial) - 1L,
    output_batch_dimension = 0L,
    output_feature_dimension = n_spatial + 1L,
    output_spatial_dimensions = seq_len(n_spatial)
  )
}

test_that("basic 2D convolution (NHWC, valid padding)", {
  func <- local_func()

  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L))

  z <- hlo_convolution(
    lhs,
    rhs,
    dimension_numbers = make_dn_nhwc(),
    window_strides = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  prog <- pjrt_program(repr(f))
  exec <- pjrt_compile(prog)

  x <- array(as.double(1:16), dim = c(1L, 4L, 4L, 1L))
  k <- array(1, dim = c(3L, 3L, 1L, 1L))
  out <- pjrt_execute(exec, pjrt_buffer(x, "f32"), pjrt_buffer(k, "f32"))

  # 3x3 sum over each 4x4 window with stride 1 -> 2x2 output
  expected <- array(c(54, 63, 90, 99), dim = c(1L, 2L, 2L, 1L))
  expect_equal(as_array(out), expected, tolerance = 1e-5)
})

test_that("2D convolution with strides and padding", {
  func <- local_func()

  lhs <- hlo_input("lhs", "f32", shape = c(1L, 5L, 5L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L))

  z <- hlo_convolution(
    lhs,
    rhs,
    dimension_numbers = make_dn_nhwc(),
    window_strides = c(2L, 2L),
    padding = matrix(c(1L, 1L, 1L, 1L), nrow = 2L, ncol = 2L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  prog <- pjrt_program(repr(f))
  exec <- pjrt_compile(prog)
  expect_class(exec, "PJRTLoadedExecutable")
})

test_that("convolution with feature_group_count (depthwise)", {
  func <- local_func()

  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 4L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 4L))

  z <- hlo_convolution(
    lhs,
    rhs,
    dimension_numbers = make_dn_nhwc(),
    window_strides = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L),
    feature_group_count = 4L
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  prog <- pjrt_program(repr(f))
  exec <- pjrt_compile(prog)
  expect_class(exec, "PJRTLoadedExecutable")
})

test_that("convolution with non-default precision_config", {
  func <- local_func()

  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L))

  z <- hlo_convolution(
    lhs,
    rhs,
    dimension_numbers = make_dn_nhwc(),
    window_strides = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L),
    precision_config = c("HIGH", "HIGHEST")
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))
})

test_that("rank-3 (1D) convolution", {
  func <- local_func()

  lhs <- hlo_input("lhs", "f32", shape = c(1L, 8L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 1L, 1L))

  z <- hlo_convolution(
    lhs,
    rhs,
    dimension_numbers = make_dn_nhwc(n_spatial = 1L),
    window_strides = 1L,
    padding = matrix(0L, nrow = 1L, ncol = 2L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  prog <- pjrt_program(repr(f))
  exec <- pjrt_compile(prog)
  expect_class(exec, "PJRTLoadedExecutable")
})

test_that("ConvDimensionNumbers repr", {
  dn <- make_dn_nhwc()
  expect_snapshot(cat(repr(dn)))
})

test_that("PrecisionConfig repr", {
  pc <- PrecisionConfig(c("DEFAULT", "DEFAULT"))
  expect_snapshot(cat(repr(pc)))
  pc2 <- PrecisionConfig(c("HIGH", "HIGHEST"))
  expect_snapshot(cat(repr(pc2)))
})

test_that("error messages", {
  check <- function(
    shape_lhs = c(1L, 4L, 4L, 1L),
    shape_rhs = c(3L, 3L, 1L, 1L),
    dimension_numbers = make_dn_nhwc(),
    window_strides = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L),
    lhs_dilation = NULL,
    rhs_dilation = NULL,
    window_reversal = NULL,
    feature_group_count = 1L,
    batch_group_count = 1L,
    precision_config = c("DEFAULT", "DEFAULT")
  ) {
    local_func()
    lhs <- hlo_input("lhs", "f32", shape = shape_lhs)
    rhs <- hlo_input("rhs", "f32", shape = shape_rhs)
    expect_snapshot_error({
      hlo_convolution(
        lhs,
        rhs,
        dimension_numbers = dimension_numbers,
        window_strides = window_strides,
        padding = padding,
        lhs_dilation = lhs_dilation,
        rhs_dilation = rhs_dilation,
        window_reversal = window_reversal,
        feature_group_count = feature_group_count,
        batch_group_count = batch_group_count,
        precision_config = precision_config
      )
    })
  }

  # C1: rank mismatch
  check(shape_lhs = c(1L, 4L, 4L, 1L), shape_rhs = c(3L, 3L, 1L))
  # C2: window_strides wrong length
  check(window_strides = c(1L, 1L, 1L))
  # C3: window_strides not positive
  check(window_strides = c(0L, 1L))
  # C4: padding wrong shape
  check(padding = matrix(0L, nrow = 1L, ncol = 2L))
  # C6: lhs_dilation not positive
  check(lhs_dilation = c(0L, 1L))
  # C8: rhs_dilation not positive
  check(rhs_dilation = c(1L, 0L))
  # C11: input_feature not divisible by feature_group_count
  check(
    shape_lhs = c(1L, 4L, 4L, 3L),
    shape_rhs = c(3L, 3L, 1L, 2L),
    feature_group_count = 2L
  )
  # C14: kernel_input_feature_dimension mismatch
  check(
    shape_lhs = c(1L, 4L, 4L, 4L),
    shape_rhs = c(3L, 3L, 1L, 1L)
  )
  # C21: feature_group_count not positive
  check(feature_group_count = 0L)
  # C22: batch_group_count not positive
  check(batch_group_count = 0L)
  # C23: only one of feature_group_count / batch_group_count can be > 1
  check(feature_group_count = 2L, batch_group_count = 2L)
  # C24: precision_config wrong length
  check(precision_config = "DEFAULT")
  # C24/precision values: invalid precision value
  check(precision_config = c("DEFAULT", "FOO"))
  # C27: dtype mismatch handled via different inputs
})

test_that("error: dimension uniqueness", {
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L))
  bad_dn <- ConvDimensionNumbers(
    input_batch_dimension = 0L,
    input_feature_dimension = 0L, # duplicate of batch
    input_spatial_dimensions = c(1L, 2L),
    kernel_input_feature_dimension = 2L,
    kernel_output_feature_dimension = 3L,
    kernel_spatial_dimensions = c(0L, 1L),
    output_batch_dimension = 0L,
    output_feature_dimension = 3L,
    output_spatial_dimensions = c(1L, 2L)
  )
  expect_snapshot_error({
    hlo_convolution(
      lhs,
      rhs,
      dimension_numbers = bad_dn,
      window_strides = c(1L, 1L),
      padding = matrix(0L, nrow = 2L, ncol = 2L)
    )
  })
})

test_that("error: dimension out of bounds", {
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L))
  rhs <- hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L))
  bad_dn <- ConvDimensionNumbers(
    input_batch_dimension = 0L,
    input_feature_dimension = 5L, # out of range for rank 4
    input_spatial_dimensions = c(1L, 2L),
    kernel_input_feature_dimension = 2L,
    kernel_output_feature_dimension = 3L,
    kernel_spatial_dimensions = c(0L, 1L),
    output_batch_dimension = 0L,
    output_feature_dimension = 3L,
    output_spatial_dimensions = c(1L, 2L)
  )
  expect_snapshot_error({
    hlo_convolution(
      lhs,
      rhs,
      dimension_numbers = bad_dn,
      window_strides = c(1L, 1L),
      padding = matrix(0L, nrow = 2L, ncol = 2L)
    )
  })
})

test_that("error: lhs and rhs different dtypes", {
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L))
  rhs <- hlo_input("rhs", "f64", shape = c(3L, 3L, 1L, 1L))
  expect_snapshot_error({
    hlo_convolution(
      lhs,
      rhs,
      dimension_numbers = make_dn_nhwc(),
      window_strides = c(1L, 1L),
      padding = matrix(0L, nrow = 2L, ncol = 2L)
    )
  })
})
