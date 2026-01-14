test_that("can handle conditions", {
  e <- tryCatch(
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = c(0L, 1L, 0L, 2L),
      call = call("abc")
    ),
    error = function(e) {
      to_one_based(e)
    }
  )
  expect_equal(e$dimensions, c(1L, 2L, 1L, 3L))
})

test_that("to_one_based works with ErrorDimensionOutOfRange", {
  err <- error_dimension_out_of_range(
    arg = "dimension",
    dimension = 5L,
    dim_range = c(0L, 3L),
    call = call("abc"),
    signal = FALSE
  )

  converted <- to_one_based(err)

  expect_equal(converted$dimension, 6L)
  expect_equal(converted$dim_range, c(1L, 4L))
})

test_that("ErrorDimensionOutOfRange", {
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimension",
      dimension = 5L,
      dim_range = c(0L, 3L),
      call = call("abc")
    )
  )

  # Test with multiple dimensions
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimensions",
      dimension = c(0L, 1L, 5L),
      dim_range = c(0L, 3L),
      call = call("abc")
    )
  )
})

test_that("ErrorDimensionUniqueness", {
  expect_snapshot_error(
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = c(0L, 1L, 0L, 2L),
      call = call("abc")
    )
  )
})

test_that("ErrorIndexOutOfBounds", {
  expect_snapshot_error(
    error_index_out_of_bounds(
      arg = "alias_indices",
      index = 7L,
      lower = 0L,
      upper = 5L,
      call = call("abc")
    )
  )
})

test_that("ErrorPermuteIndex", {
  expect_snapshot_error(
    error_permute_index(
      arg = "permutation",
      permutation = c(0L, 2L, 1L, 3L),
      expected = c(0L, 1L, 2L),
      call = call("abc")
    )
  )
})

test_that("ErrorUnequalTypes", {
  expect_snapshot_error(
    error_unequal_types(
      arg1 = "output_types(true_branch)",
      arg2 = "output_types(false_branch)",
      index = 2L,
      expected = "must have the same type",
      actual1 = "<tensor<2x2xf32>>",
      actual2 = "<tensor<2x2xi32>>",
      call = call("abc")
    )
  )
})

test_that("ErrorUnexpectedType", {
  expect_snapshot_error(
    error_unexpected_type(
      arg = "init_values",
      index = 0L,
      expected = "must be 0-D tensors",
      actual = "shape (2, 2)",
      call = call("abc")
    )
  )
})
