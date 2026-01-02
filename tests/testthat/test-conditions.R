test_that("InvalidIdentifierError", {
  expect_snapshot_error(
    invalid_identifier_error(arg = "_name")
  )
})

test_that("UnequalTensorTypesError", {
  expect_snapshot_error(
    unequal_tensor_types_error(
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
    class_error(
      arg = "x",
      expected = c("TensorType", "TokenType"),
      observed = "ValueType"
    )
  )
})

test_that("TensorDTypeError", {
  expect_snapshot_error(
    tensor_dtype_error(
      arg = "operand",
      expected = c("i32", "i64"),
      observed = "f32"
    )
  )
})

test_that("TensorNDimsError", {
  # Test >= lower bound (c(lower, NA))
  expect_snapshot_error(
    tensor_ndims_error(
      arg = "operand",
      expected = c(0L, NA),
      observed = -1L
    )
  )

  # Test < upper bound (c(NA, upper))
  expect_snapshot_error(
    tensor_ndims_error(
      arg = "operand",
      expected = c(NA, 1L),
      observed = 2L
    )
  )

  # Test range [lower, upper)
  expect_snapshot_error(
    tensor_ndims_error(
      arg = "operand",
      expected = c(1L, 3L),
      observed = 0L
    )
  )

  # Test exact value (c(n, n+1) means exactly n)
  expect_snapshot_error(
    tensor_ndims_error(
      arg = "operand",
      expected = c(2L, 3L),
      observed = 1L
    )
  )
})

test_that("TensorShapeError", {
  expect_snapshot_error(
    tensor_shape_error(
      arg = "operand",
      expected = c(2L, 3L),
      observed = c(2L, 4L)
    )
  )
})

test_that("ShapeMismatchError", {
  expect_snapshot_error(
    shape_mismatch_error(
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
    dimension_out_of_range_error(
      arg = "dimension",
      dimension = 5L,
      ndims = 3L
    )
  )

  # Test with multiple dimensions
  expect_snapshot_error(
    dimension_out_of_range_error(
      arg = "dimensions",
      dimension = c(0L, 1L, 5L),
      ndims = 3L
    )
  )
})

test_that("DimensionUniquenessError", {
  expect_snapshot_error(
    dimension_uniqueness_error(
      arg = "dimensions",
      dimensions = c(0L, 1L, 0L, 2L)
    )
  )
})

test_that("IndexOutOfBoundsError", {
  expect_snapshot_error(
    index_out_of_bounds_error(
      arg = "alias_indices",
      lower = 0L,
      upper = 5L
    )
  )
})

test_that("SliceIndexError", {
  # Test start indices
  expect_snapshot_error(
    slice_index_error(
      arg = "start_indices",
      indices = c(-1L, 0L),
      index_type = "start"
    )
  )

  # Test limit indices
  expect_snapshot_error(
    slice_index_error(
      arg = "limit_indices",
      indices = c(10L, 20L),
      index_type = "limit"
    )
  )
})

test_that("PermutationError", {
  expect_snapshot_error(
    permutation_error(
      arg = "permutation",
      permutation = c(0L, 2L, 1L, 3L),
      ndims = 3L
    )
  )
})
