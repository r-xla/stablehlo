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
      index = 7L,
      lower = 0L,
      upper = 5L
    )
  )
})

test_that("SliceIndexError", {
  expect_snapshot_error(
    slice_index_error(
      arg = "start_indices",
      index = -1L,
      lower = 0L,
      upper = 10L
    )
  )
})

test_that("PermuteIndexError", {
  expect_snapshot_error(
    permute_index_error(
      arg = "permutation",
      permutation = c(0L, 2L, 1L, 3L),
      expected = c(0L, 1L, 2L)
    )
  )
})
