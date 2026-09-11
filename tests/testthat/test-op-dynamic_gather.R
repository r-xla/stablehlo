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

test_that("dynamic_gather refines, compiles and runs", {
  skip_if_no_refine()
  expect_dynamic_op_runs(
    build = function() {
      hlo_dynamic_gather(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("i", "i32", c(2L, 1L)),
        gather_dimension_numbers = GatherDimensionNumbers(
          offset_dims = 1L,
          collapsed_slice_dims = 0L,
          start_index_map = 0L,
          index_vector_dim = 1L
        ),
        slice_sizes = hlo_tensor(c(1L, 3L), dtype = "i32", shape = 2L)
      )
    },
    types = c("tensor<4x3xf32>", "tensor<2x1xi32>"),
    args = list(
      pjrt::pjrt_buffer(matrix(1:12 + 0, nrow = 4L), dtype = "f32"),
      pjrt::pjrt_buffer(matrix(c(0L, 2L), ncol = 1L), dtype = "i32")
    ),
    inferred_type = "tensor<2x?xf32>",
    refined_type = "tensor<2x3xf32>",
    expected = as.vector(matrix(1:12 + 0, nrow = 4L)[c(1L, 3L), ])
  )
})
