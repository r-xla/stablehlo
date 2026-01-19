test_that("scatter looks correct", {
  # snapshot tests don't work in `it()` blocks for some reason
  func <- local_func()
  input <- hlo_input("input", "i32", c(4L))
  scatter_indices <- hlo_input("scatter_indices", "i32", c(2L, 1L))
  # Update shape: [2] for scatter dims (2 scatter positions, each updating 1 element)
  update <- hlo_input("update", "i32", c(2L))

  # Create update computation: add
  update_func <- local_func("update")
  a <- hlo_input("a", "i32", integer())
  b <- hlo_input("b", "i32", integer())
  sum_result <- hlo_add(a, b)
  update_func <- hlo_return(sum_result)

  scatter_dim_numbers <- ScatterDimensionNumbers(
    update_window_dims = integer(),
    inserted_window_dims = 0L,
    scatter_dims_to_operand_dims = 0L,
    index_vector_dim = 1L
  )

  result <- hlo_scatter(
    inputs = list(input),
    scatter_indices = scatter_indices,
    updates = list(update),
    scatter_dimension_numbers = scatter_dim_numbers,
    update_computation = update_func
  )

  func <- hlo_return(result)
  expect_snapshot(repr(func))
})

describe("scatter", {
  skip_if_not_installed("pjrt")

  check <- function(
    input,
    scatter_indices,
    update,
    update_window_dims,
    inserted_window_dims,
    scatter_dims_to_operand_dims,
    index_vector_dim,
    input_batching_dims = integer(),
    scatter_indices_batching_dims = integer(),
    indices_are_sorted = FALSE,
    unique_indices = FALSE,
    expected
  ) {
    local_func()

    to_int <- function(x) {
      y <- as.integer(x)
      if (!is.null(dim(x))) {
        dim(y) <- dim(x)
      }
      y
    }

    if (!inherits(input, "PJRTBuffer")) {
      input <- pjrt_buffer(to_int(input), dtype = "i64")
    }
    if (!inherits(update, "PJRTBuffer")) {
      update <- pjrt_buffer(to_int(update), dtype = "i64")
    }
    if (!inherits(scatter_indices, "PJRTBuffer")) {
      scatter_indices <- pjrt_buffer(to_int(scatter_indices), dtype = "i64")
    }

    input_arg <- hlo_input("input", "i64", shape(input))
    scatter_indices_arg <- hlo_input(
      "scatter_indices",
      "i64",
      shape(scatter_indices)
    )
    update_arg <- hlo_input("update", "i64", shape(update))

    local_func("update_fn")
    a <- hlo_input("a", "i64")
    b <- hlo_input("b", "i64")
    result <- hlo_add(a, b)
    update_fn <- hlo_return(result)

    scatter_dim_numbers <- ScatterDimensionNumbers(
      update_window_dims = update_window_dims,
      inserted_window_dims = inserted_window_dims,
      input_batching_dims = input_batching_dims,
      scatter_indices_batching_dims = scatter_indices_batching_dims,
      scatter_dims_to_operand_dims = scatter_dims_to_operand_dims,
      index_vector_dim = index_vector_dim
    )

    r <- hlo_scatter(
      inputs = input_arg,
      scatter_indices = scatter_indices_arg,
      updates = update_arg,
      scatter_dimension_numbers = scatter_dim_numbers,
      indices_are_sorted = indices_are_sorted,
      unique_indices = unique_indices,
      update_computation = update_fn
    )

    func <- hlo_return(r)
    program <- pjrt_program(repr(func))
    executable <- pjrt_compile(program)
    out_buf <- pjrt_execute(executable, input, scatter_indices, update)
    expected <- pjrt_buffer(expected, dtype = "i64")
    expect_equal(
      out_buf,
      expected
    )
  }

  it("can update element in vector", {
    # Update single element at index 0: input[0] += -1, so 1 + (-1) = 0
    check(
      input = 1:3,
      scatter_indices = 0L,
      update = row_major_array(-1L, dim = 1L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(0L, 2L, 3L)
    )
    # Update single element at index 1: input[1] += -1, so 2 + (-1) = 1
    check(
      input = 1:3,
      scatter_indices = 1L,
      update = row_major_array(-1L, dim = 1L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(1L, 1L, 3L)
    )
    # Update with enumeration dimensions - scatter_indices has extra dims
    check(
      input = 1:3,
      scatter_indices = row_major_array(1L, dim = c(1L, 1L)),
      update = row_major_array(-1L, dim = c(1L)),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(1L, 1L, 3L)
    )
  })
  # it("can update a slice of a vector", {

  #   # we update input[2:3]
  #   check(
  #     input = 1:4,
  #     scatter_indices = 1L,
  #     update =
  #   )
  # })
  # TODO: This test requires partial window updates which isn't supported
  # by current StableHLO validation. The update window size must equal
  # the full input dimension minus inserted_window_dims.
  # it("can update two slices in the same axis", {
  #   check(
  #     input = 1:10,
  #     scatter_indices = c(0L, 3L),
  #     update = matrix(c(1L, 2L, -1L, -2L), nrow = 2L, byrow = TRUE),
  #     update_window_dims = 1L,
  #     inserted_window_dims = integer(),
  #     scatter_dims_to_operand_dims = 0L,
  #     index_vector_dim = 1L,
  #     expected = c(2L, 4L, 3L, 3L, 3L, 6L, 7L, 8L, 9L, 10L)
  #   )
  # })
  it("works with implicit and explicit index vector dimension", {
    vec <- c(1L, 2L, 3L, 4L)

    check(
      input = vec,
      scatter_indices = c(1L, 2L),
      index_vector_dim = 1L, # Equal to rank(scatter_indices), so implicit
      update = c(10L, 10L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      expected = c(1L, 12L, 13L, 4L)
    )

    check(
      input = vec,
      scatter_indices = matrix(c(1L, 2L), nrow = 1L),
      index_vector_dim = 0L,
      update = c(10L, 10L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      expected = c(1L, 12L, 13L, 4L)
    )

    check(
      input = vec,
      scatter_indices = matrix(c(1L, 2L), ncol = 1L),
      index_vector_dim = 1L,
      update = c(10L, 10L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      expected = c(1L, 12L, 13L, 4L)
    )
  })
  # TODO: These tests require partial window updates which isn't fully supported
  # by current StableHLO validation semantics.
  it("can update column(s) in 2D matrix", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    check(
      input = mat,
      scatter_indices = matrix(c(0L, 2L), nrow = 1L),
      index_vector_dim = 1L,
      update = row_major_array(rep(1L, 3L), dim = c(1L, 3L, 1L)),
      update_window_dims = c(1L, 2L),
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = c(0L, 1L),
      expected = matrix(c(1, 2, 4, 4, 5, 7, 7, 8, 10), nrow = 3L, byrow = TRUE)
    )
  })
  #TODO: This test requires partial window updates which isn't fully supported
  #by current StableHLO validation semantics.
  it("different representations of update (parameterized by update_window_dims)", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    check2 <- function(update_dims, update_window_dims) {
      check(
        input = mat,
        scatter_indices = matrix(c(0L, 2L), nrow = 1L),
        index_vector_dim = 1L,
        update = row_major_array(rep(1L, 3L), dim = update_dims),
        update_window_dims = update_window_dims,
        inserted_window_dims = integer(),
        scatter_dims_to_operand_dims = c(0L, 1L),
        expected = matrix(
          c(1, 2, 4, 4, 5, 7, 7, 8, 10),
          nrow = 3L,
          byrow = TRUE
        )
      )
    }
    check2(c(1L, 3L, 1L), c(1L, 2L))
    check2(c(3L, 1L, 1L), c(0L, 1L))
  })
  it("works with inserted_window_dims", {
    # Let's say we have a 4D tensor and we want to insert a few scalars.
    # We have 3D scatters, one enumeration dimension, no batch dimension
    # I.e., the updates are in principle of rank size(enum_dims) + 3L
    # But all 3 dims are scalar, i.e. no window
  })
  it("works with empty update_window_dims", {
    check(
      input = 1:3,
      scatter_indices = 0L,
      update = row_major_array(1L, dim = 1L),
      update_window_dims = integer(),
      inserted_window_dims = 0L,
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(2L, 2L, 3L)
    )
  })
  it("works with non-ordered scatter_dims_to_operand_dims", {
    mat <- matrix(1:4, nrow = 2L, byrow = TRUE)
    check(
      input = mat,
      scatter_indices = c(0L, 1L), # but update (1L, 0L)
      update = pjrt_scalar(1L, dtype = "i64"),
      inserted_window_dims = c(0L, 1L),
      update_window_dims = integer(),
      scatter_dims_to_operand_dims = c(1L, 0L),
      index_vector_dim = 0L,
      expected = matrix(c(1L, 2L, 4L, 4L), nrow = 2L, byrow = TRUE)
    )
  })
  it("works with inserted_window_dims", {
    # We update a single element in a 2D matrix
    mat <- matrix(1:4, nrow = 2L, byrow = TRUE)
    check(
      input = mat,
      scatter_indices = matrix(c(1L, 0L), nrow = 1L),
      update = row_major_array(10L, dim = 1L),
      inserted_window_dims = c(0L, 1L),
      update_window_dims = integer(),
      scatter_dims_to_operand_dims = c(0L, 1L),
      index_vector_dim = 1L,
      expected = matrix(c(1L, 2L, 13L, 4L), nrow = 2L, byrow = TRUE)
    )
  })
  it("works with no iteration dimensions", {
    mat <- matrix(1:4, nrow = 2L, byrow = TRUE)
    check(
      input = mat,
      scatter_indices = c(1L, 0L),
      update = pjrt_buffer(-1L, dtype = "i64"),
      inserted_window_dims = 1L,
      update_window_dims = 0L,
      scatter_dims_to_operand_dims = c(0L, 1L),
      index_vector_dim = 0L,
      expected = matrix(c(1L, 2L, 2L, 4L), nrow = 2L, byrow = TRUE)
    )
  })
  it("works with scalar update", {
    mat <- matrix(1:4, nrow = 2L, byrow = TRUE)
    check(
      input = mat,
      scatter_indices = c(1L, 0L),
      update = pjrt_scalar(-1L, dtype = "i64"),
      inserted_window_dims = c(0L, 1L),
      update_window_dims = integer(),
      scatter_dims_to_operand_dims = c(0L, 1L),
      index_vector_dim = 0L,
      expected = matrix(c(1L, 2L, 2L, 4L), nrow = 2L, byrow = TRUE)
    )
  })
  it("works with batching dimension", {
    mat <- matrix(1:6, nrow = 2L, byrow = TRUE)
    update <- row_major_array(c(1, 3L, 2L, 4L), dim = c(1, 2, 2))
    mat <- matrix(
      c(1, 2, 3, 4, 5, 6),
      nrow = 2L,
      byrow = TRUE
    )
    expected <- matrix(
      c(1L, 3L, 6L, 6L, 9L, 6L),
      nrow = 2L,
      byrow = TRUE
    )
    check(
      input_batching_dims = 0L,
      scatter_indices_batching_dims = 1L,
      input = mat,
      scatter_indices = matrix(c(1L, 0L), nrow = 1L),
      update = update,
      inserted_window_dims = integer(),
      update_window_dims = 2L,
      scatter_dims_to_operand_dims = 1L,
      index_vector_dim = 2L,
      expected = expected
    )
  })
  it("works with example from spec", {
    # fmt: skip
    input <- row_major_array(c(
       1,  2,  3,  4,  5,  6,  7,  8, # nolint
       9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      25, 26, 27, 28, 29, 30, 31, 32,
      33, 34, 35, 36, 37, 38, 39, 40,
      41, 42, 43, 44, 45, 46, 47, 48
    ), dim = c(2L, 4L, 3L, 2L))

    # fmt: skip
    scatter_indices <- row_major_array(c(
      0, 0, 1, 0, 2, 1,
      0, 1, 1, 1, 0, 9,
      0, 0, 2, 1, 2, 2,
      1, 2, 0, 1, 1, 0
    ), dim = c(2L, 3L, 2L, 2L))

    update <- row_major_array(1L, dim = c(2L, 2L, 3L, 2L, 2L))

    # fmt: skip
    expected <- array(c(
       3L, 27L, 10L, 34L, 14L, 38L, 19L, 43L, # nolint
       3L, 27L, 10L, 34L, 16L, 40L, 21L, 45L,
       5L, 29L, 12L, 36L, 19L, 43L, 24L, 48L,
       2L, 26L,  8L, 32L, 14L, 38L, 20L, 44L,
       7L, 31L, 13L, 37L, 16L, 40L, 22L, 46L,
       6L, 30L, 14L, 38L, 20L, 44L, 24L, 48L
    ), dim = c(2L, 4L, 3L, 2L))

    check(
      input = input,
      scatter_indices = scatter_indices,
      update = update,
      update_window_dims = c(0L, 1L),
      inserted_window_dims = 2L,
      scatter_dims_to_operand_dims = c(1L, 2L),
      index_vector_dim = 0L,
      input_batching_dims = 3L,
      scatter_indices_batching_dims = 2L,
      expected = expected
    )
  })
  it("works with additional examples from spec", {
    # fmt: skip
    input <- row_major_array(
      c(
        1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24
      ),
      dim = c(2L, 4L, 3L)
    )

    # fmt: skip
    scatter_indices <- row_major_array(c(
      0, 2, 1, 0, 2, 1,
      0, 1, 1, 0, 0, 9
    ), dim = c(2L, 3L, 2L))

    update <- row_major_array(1L, dim = c(2L, 2L, 3L, 2L))

    # fmt: skip
    expected <- array(c(
       3L, 15L,  6L, 18L,  8L, 20L, 11L, 23L, # nolint
       2L, 14L,  6L, 18L, 10L, 22L, 12L, 24L,
       3L, 15L,  6L, 18L,  9L, 21L, 12L, 24L
    ), dim = c(2L, 4L, 3L))

    check(
      input = input,
      scatter_indices = scatter_indices,
      update = update,
      update_window_dims = c(0L, 1L),
      inserted_window_dims = 2L,
      scatter_dims_to_operand_dims = c(1L, 2L),
      index_vector_dim = 0L,
      expected = expected
    )
  })
})
