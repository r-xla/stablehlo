test_that("InvalidIdentifierError", {
  expect_snapshot_error(
    error_invalid_identifier(arg = "_name")
  )
})

test_that("UnequalTensorTypesError", {
  expect_snapshot_error(
    error_unequal_tensor_types(
      args = setNames(
        list(
          TensorType(dtype = IntegerType(32L), shape = Shape(c(2L, 3L))),
          TensorType(dtype = FloatType(32L), shape = Shape(c(2L, 3L)))
        ),
        c("x", "y")
      )
    )
  )
})

test_that("ClassError", {
  expect_snapshot_error(
    error_class(
      arg = "x",
      expected = c("TensorType", "TokenType"),
      observed = "ValueType"
    )
  )
})

test_that("TensorDTypeError", {
  expect_snapshot_error(
    error_tensor_dtype(
      arg = "operand",
      expected = c("i32", "i64"),
      observed = "f32"
    )
  )
})

test_that("TensorNDimsError", {
  # Test >= lower bound (c(lower, NA))
  expect_snapshot_error(
    error_tensor_ndims(
      arg = "operand",
      expected = c(0L, NA),
      observed = -1L
    )
  )

  # Test < upper bound (c(NA, upper))
  expect_snapshot_error(
    error_tensor_ndims(
      arg = "operand",
      expected = c(NA, 1L),
      observed = 2L
    )
  )

  # Test range [lower, upper)
  expect_snapshot_error(
    error_tensor_ndims(
      arg = "operand",
      expected = c(1L, 3L),
      observed = 0L
    )
  )

  # Test exact value (c(n, n+1) means exactly n)
  expect_snapshot_error(
    error_tensor_ndims(
      arg = "operand",
      expected = c(2L, 3L),
      observed = 1L
    )
  )
})

test_that("TensorShapeError", {
  expect_snapshot_error(
    error_tensor_shape(
      arg = "operand",
      expected = c(2L, 3L),
      observed = c(2L, 4L)
    )
  )
})

test_that("ShapeMismatchError", {
  expect_snapshot_error(
    error_shape_mismatch(
      arg_lhs = "lhs",
      arg_rhs = "rhs",
      dim_lhs = 0L,
      dim_rhs = 1L,
      size_lhs = 10L,
      size_rhs = 20L
    )
  )
})

test_that("DimensionOutOfRangeError", {
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimension",
      dimension = 5L,
      ndims = 3L
    )
  )

  # Test with multiple dimensions
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimensions",
      dimension = c(0L, 1L, 5L),
      ndims = 3L
    )
  )
})

test_that("DimensionUniquenessError", {
  expect_snapshot_error(
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = c(0L, 1L, 0L, 2L)
    )
  )
})

test_that("IndexOutOfBoundsError", {
  expect_snapshot_error(
    error_index_out_of_bounds(
      arg = "alias_indices",
      lower = 0L,
      upper = 5L
    )
  )
})

test_that("SliceIndexError", {
  # Test start indices
  expect_snapshot_error(
    error_slice_index(
      arg = "start_indices",
      indices = c(-1L, 0L),
      index_type = "start"
    )
  )

  # Test limit indices
  expect_snapshot_error(
    error_slice_index(
      arg = "limit_indices",
      indices = c(10L, 20L),
      index_type = "limit"
    )
  )
})

test_that("PermutationError", {
  expect_snapshot_error(
    error_permutation(
      arg = "permutation",
      permutation = c(0L, 2L, 1L, 3L),
      ndims = 3L
    )
  )
})

test_that("error conditions can be created without signaling", {
  skip_on_cran()

  # Test that signal = FALSE returns a condition object
  err <- error_class("x", "numeric", "character", signal = FALSE)
  expect_s3_class(err, "class_error")
  expect_s3_class(err, "stablehlo_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")

  # Verify the condition contains the expected fields
  expect_equal(err$arg, "x")
  expect_equal(err$expected, "numeric")
  expect_equal(err$observed, "character")
})
