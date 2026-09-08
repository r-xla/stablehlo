# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_conv", {
  # Batch and feature axes stay static; the spatial extents depend on the
  # padding, which is now data.
  expect_equal(
    inferred(function() {
      hlo_dynamic_conv(
        dyn_input("a", "f32", c(2L, 1L, 4L)),
        dyn_input("k", "f32", c(1L, 1L, 2L)),
        padding = dyn_input("p", "i32", c(1L, 2L)),
        dimension_numbers = ConvDimensionNumbers(
          0L,
          1L,
          2L,
          1L,
          0L,
          2L,
          0L,
          1L,
          2L
        ),
        window_strides = 1L
      )
    }),
    "tensor<2x1x?xf32>"
  )
  # A dynamic batch axis stays dynamic too.
  expect_equal(
    inferred(function() {
      hlo_dynamic_conv(
        dyn_input("a", "f32", c(N, 1L, 4L)),
        dyn_input("k", "f32", c(1L, 1L, 2L)),
        padding = dyn_input("p", "i32", c(1L, 2L)),
        dimension_numbers = ConvDimensionNumbers(
          0L,
          1L,
          2L,
          1L,
          0L,
          2L,
          0L,
          1L,
          2L
        ),
        window_strides = 1L
      )
    }),
    "tensor<?x1x?xf32>"
  )
  # `padding` must have one row per spatial axis.
  local_func()
  expect_error(
    hlo_dynamic_conv(
      dyn_input("a", "f32", c(2L, 1L, 4L)),
      dyn_input("k", "f32", c(1L, 1L, 2L)),
      padding = dyn_input("p", "i32", c(3L, 2L)),
      dimension_numbers = ConvDimensionNumbers(
        0L,
        1L,
        2L,
        1L,
        0L,
        2L,
        0L,
        1L,
        2L
      ),
      window_strides = 1L
    ),
    "one row per spatial axis"
  )
  # ... and one inside the delegate: the feature axes must divide by
  # `feature_group_count` (convolution C11), which `dynamic_conv` does not
  # check itself.
  local_func()
  expect_error(
    hlo_dynamic_conv(
      dyn_input("a", "f32", c(2L, 3L, 4L)),
      dyn_input("k", "f32", c(1L, 3L, 2L)),
      padding = dyn_input("p", "i32", c(1L, 2L)),
      dimension_numbers = ConvDimensionNumbers(
        0L,
        1L,
        2L,
        1L,
        0L,
        2L,
        0L,
        1L,
        2L
      ),
      window_strides = 1L,
      feature_group_count = 2L
    ),
    "divisible"
  )
})
