# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_gather", {
  # The batch axes come from `start_indices` and stay static; the offset axes
  # come from `slice_sizes`, which is now data, so they are `?`.
  expect_equal(
    inferred(function() {
      hlo_dynamic_gather(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("i", "i32", c(2L, 1L)),
        slice_sizes = dyn_input("sz", "i32", 2L),
        gather_dimension_numbers = GatherDimensionNumbers(
          offset_dims = 1L,
          collapsed_slice_dims = 0L,
          start_index_map = 0L,
          index_vector_dim = 1L
        )
      )
    }),
    "tensor<2x?xf32>"
  )
  # It still enforces everything `gather` does -- the inference is shared, with
  # the extents marked unknown.
  local_func()
  expect_error(
    hlo_dynamic_gather(
      dyn_input("a", "f32", c(N, 3L)),
      dyn_input("i", "i32", c(2L, 1L)),
      slice_sizes = dyn_input("sz", "i32", 5L),
      gather_dimension_numbers = GatherDimensionNumbers(
        offset_dims = 1L,
        collapsed_slice_dims = 0L,
        start_index_map = 0L,
        index_vector_dim = 1L
      )
    ),
    "one element per axis"
  )
  # ... and a check that lives *inside* the delegate, reached only because the
  # inference is shared: `offset_dims` must be sorted (gather C4). Nothing in
  # `dynamic_gather` itself checks this.
  local_func()
  expect_error(
    hlo_dynamic_gather(
      dyn_input("a", "f32", c(N, 3L, 2L)),
      dyn_input("i", "i32", c(2L, 1L)),
      slice_sizes = dyn_input("sz", "i32", 3L),
      gather_dimension_numbers = GatherDimensionNumbers(
        offset_dims = c(2L, 1L),
        collapsed_slice_dims = 0L,
        start_index_map = 0L,
        index_vector_dim = 1L
      )
    ),
    "sorted"
  )
})
