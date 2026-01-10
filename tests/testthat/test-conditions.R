test_that("can handle conditions", {
  x <- tryCatch(
    error_dimension_uniqueness(
      arg = "dimensions",
      dimensions = c(0L, 1L, 0L, 2L),
      call = call("abc")
    ),
    error = function(e) {
      to_one_based(e)
    }
  )
})

test_that("ErrorDimensionOutOfRange", {
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimension",
      dimension = 5L,
      call = call("abc"),
      ndims = 3L
    )
  )

  # Test with multiple dimensions
  expect_snapshot_error(
    error_dimension_out_of_range(
      arg = "dimensions",
      dimension = c(0L, 1L, 5L),
      call = call("abc"),
      ndims = 3L
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

test_that("ErrorSliceIndex", {
  expect_snapshot_error(
    error_slice_index(
      arg = "start_indices",
      index = -1L,
      lower = 0L,
      upper = 10L,
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
