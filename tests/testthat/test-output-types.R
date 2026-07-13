# When a caller passes `output_types`, `hlo_fn()` skips type inference (and the
# input validation it performs) and uses the supplied types directly. These
# tests cover the threading of `output_types` through the hlo_* builders.

# repr of a one-op program, where `make()` returns the op's FuncValue
repr_after <- function(make) {
  local_func()
  repr(hlo_return(make()))
}

# the value type(s) inference assigns to `make()`'s result, as an output_types list
inferred_types <- function(make) {
  local_func()
  list(make()$value_type)
}

# a 2D NHWC convolution, the simplest op carrying custom_attrs
conv_2d <- function(output_types = NULL) {
  hlo_convolution(
    hlo_input("lhs", "f32", shape = c(1L, 4L, 4L, 1L)),
    hlo_input("rhs", "f32", shape = c(3L, 3L, 1L, 1L)),
    dimension_numbers = ConvDimensionNumbers(
      input_batch_dimension = 0L,
      input_feature_dimension = 3L,
      input_spatial_dimensions = c(1L, 2L),
      kernel_input_feature_dimension = 2L,
      kernel_output_feature_dimension = 3L,
      kernel_spatial_dimensions = c(0L, 1L),
      output_batch_dimension = 0L,
      output_feature_dimension = 3L,
      output_spatial_dimensions = c(1L, 2L)
    ),
    window_strides = c(1L, 1L),
    padding = matrix(0L, nrow = 2L, ncol = 2L),
    output_types = output_types
  )
}

test_that("output_types path produces identical IR to inference (all shapes)", {
  cases <- list(
    "floor (unary elementwise)" = list(
      infer = function() hlo_floor(hlo_input("x", "f32", shape = c(2, 3))),
      ot = function(ot) {
        hlo_floor(hlo_input("x", "f32", shape = c(2, 3)), output_types = ot)
      }
    ),
    "and (binary elementwise)" = list(
      infer = function() {
        hlo_and(
          hlo_input("x", "pred", shape = c(2, 3)),
          hlo_input("y", "pred", shape = c(2, 3))
        )
      },
      ot = function(ot) {
        hlo_and(
          hlo_input("x", "pred", shape = c(2, 3)),
          hlo_input("y", "pred", shape = c(2, 3)),
          output_types = ot
        )
      }
    ),
    "slice (op with attributes)" = list(
      infer = function() {
        hlo_slice(
          hlo_input("x", "f32", shape = 4),
          start_indices = 0,
          limit_indices = 2,
          strides = 1
        )
      },
      ot = function(ot) {
        hlo_slice(
          hlo_input("x", "f32", shape = 4),
          start_indices = 0,
          limit_indices = 2,
          strides = 1,
          output_types = ot
        )
      }
    ),
    "acos (CHLO op)" = list(
      infer = function() hlo_acos(hlo_input("x", "f32", shape = c(2, 3))),
      ot = function(ot) {
        hlo_acos(hlo_input("x", "f32", shape = c(2, 3)), output_types = ot)
      }
    ),
    "convolution (op with custom_attrs)" = list(
      infer = function() conv_2d(),
      ot = function(ot) conv_2d(output_types = ot)
    )
  )

  for (nm in names(cases)) {
    case <- cases[[nm]]
    ot <- inferred_types(case$infer)
    expect_identical(
      repr_after(case$infer),
      repr_after(function() case$ot(ot)),
      info = nm
    )
  }
})

test_that("supplying output_types skips inference and its input validation", {
  local_func()
  a <- hlo_input("a", "f32", shape = c(2, 3))
  b <- hlo_input("b", "f32", shape = c(3, 2))

  # inference validates that operands share a type
  expect_error(hlo_add(a, b))

  # with output_types provided, inference (and the check above) is skipped
  ot <- list(ValueType("f32", shape = c(2, 3)))
  expect_no_error(hlo_add(a, b, output_types = ot))
})
