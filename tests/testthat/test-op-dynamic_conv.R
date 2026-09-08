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

test_that("dynamic_conv refines, compiles and runs", {
  skip_if_no_refine()
  # A 1x1x2 kernel of ones over a 2x1x4 input: each output is the sum of an
  # adjacent pair along the spatial axis.
  x <- array(1:8 + 0, dim = c(2L, 1L, 4L))
  expect_dynamic_op_runs(
    build = function() {
      hlo_dynamic_conv(
        dyn_input("a", "f32", c(N, 1L, 4L)),
        dyn_input("k", "f32", c(1L, 1L, 2L)),
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
        padding = hlo_tensor(
          matrix(0L, 1L, 2L),
          dtype = "i32",
          shape = c(1L, 2L)
        )
      )
    },
    types = c("tensor<2x1x4xf32>", "tensor<1x1x2xf32>"),
    args = list(
      pjrt::pjrt_buffer(x, dtype = "f32"),
      pjrt::pjrt_buffer(array(c(1, 1), dim = c(1L, 1L, 2L)), dtype = "f32")
    ),
    refined_type = "tensor<2x1x3xf32>",
    expected = as.vector(vapply(
      seq_len(3L),
      function(j) x[, 1L, j] + x[, 1L, j + 1L],
      numeric(2L)
    ))
  )
})
