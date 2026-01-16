describe("scatter", {
  it("looks correct in snapshot", {})
  skip_if_not_installed("pjrt")

  # Helper to create arrays with explicit row-major structure
  arr <- function(data, dim) {
    aperm(array(data, dim = dim), perm = seq_along(dim))
  }

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
    check(
      input = 1:3,
      scatter_indices = 0L,
      update = matrix(-1L),
      update_window_dims = 0L,
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(0L, 2, 3L)
    )
    check(
      input = 1:3,
      scatter_indices = 1L,
      update = matrix(-1L),
      update_window_dims = 0L,
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L,
      expected = c(1L, 1L, 3L)
    )
    # We can add enumeration dimensions to the update and the scatter indices.
    check(
      input = 1:3,
      scatter_indices = array(1L, dim = c(1, 1, 1)),
      update = array(-1L, dim = c(1, 1, 1)),
      update_window_dims = 2L,
      inserted_window_dims = integer(),
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
  it("can update two slices in the same axis", {
    # We update the first dimension twice: once starting at 0 and once starting at 3
    # both update windows are of size 2
    check(
      input = 1:10,
      scatter_indices = c(0L, 3L),
      update = matrix(c(1L, 2L, -1L, -2L), nrow = 2L, byrow = TRUE),
      update_window_dims = 1L, # first dimensions is enumeration dimension
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = 0L,
      index_vector_dim = 1L, # implicitly appended to scatter_indices
      expected = c(2L, 4L, 3L, 3L, 3L, 6L, 7L, 8L, 9L, 10L)
    )
  })
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
  it("can update column(s) in 2D matrix", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    # update third column
    check(
      input = mat,
      scatter_indices = matrix(c(0L, 2L), nrow = 1L),
      index_vector_dim = 1L,
      update = array(rep(1L, 3L), dim = c(1L, 3L, 1L)),
      update_window_dims = c(1L, 2L),
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = c(0L, 1L),
      expected = matrix(c(1, 2, 4, 4, 5, 7, 7, 8, 10), nrow = 3L, byrow = TRUE)
    )
    # update second and third column
    check(
      input = mat,
      scatter_indices = matrix(c(0L, 2L, 0L, 1L), nrow = 2L, byrow = TRUE),
      index_vector_dim = 1L,
      update = array(1L, dim = c(2L, 3L, 1L)),
      update_window_dims = c(1L, 2L),
      inserted_window_dims = integer(),
      scatter_dims_to_operand_dims = c(0L, 1L),
      expected = matrix(c(1, 3, 4, 4, 6, 7, 7, 9, 10), nrow = 3L, byrow = TRUE)
    )
  })
  it("different representations of update (parameterized by update_window_dims)", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    # update third column
    check2 <- function(update_dims, update_window_dims) {
      check(
        input = mat,
        scatter_indices = matrix(c(0L, 2L), nrow = 1L),
        index_vector_dim = 1L,
        update = array(rep(1L, 3L), dim = update_dims),
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

    # we can decide freely where to put the updates (but they most be sorted)
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
      update = array(1L, dim = 1L),
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
      update = array(10L, dim = 1L),
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
    update <- array(c(1, 3L, 2L, 4L), dim = c(1, 2, 2))
    mat <- matrix(
      c(1, 2, 3, 4, 5, 6),
      nrow = 2L,
      byrow = TRUE
    )
    expected <- matrix(
      c(1L, 2 + 1L, 3 + 2L, 4 + 3L, 5 + 4L, 6L),
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
    input <- arr(
      c(
        c(c(1, 2), c(3, 4), c(5, 6), c(7, 8)),
        c(c(9, 10), c(11, 12), c(13, 14), c(15, 16)),
        c(c(17, 18), c(19, 20), c(21, 22), c(23, 24)),
        c(c(25, 26), c(27, 28), c(29, 30), c(31, 32)),
        c(c(33, 34), c(35, 36), c(37, 38), c(39, 40)),
        c(c(41, 42), c(43, 44), c(45, 46), c(47, 48))
      ),
      dim = c(2L, 4L, 3L, 2L)
    )

    scatter_indices <- arr(
      c(
        c(c(0, 0), c(1, 0), c(2, 1)),
        c(c(0, 1), c(1, 1), c(0, 9)),
        c(c(0, 0), c(2, 1), c(2, 2)),
        c(c(1, 2), c(0, 1), c(1, 0))
      ),
      dim = c(2L, 3L, 2L, 2L)
    )

    update <- array(1L, dim = c(2L, 2L, 3L, 2L, 2L))

    expected <- arr(
      c(
        c(c(3, 4), c(6, 7), c(6, 7), c(7, 8)),
        c(c(9, 10), c(11, 12), c(15, 16), c(17, 18)),
        c(c(17, 18), c(19, 20), c(22, 23), c(24, 25)),
        c(c(25, 26), c(28, 29), c(30, 31), c(31, 32)),
        c(c(35, 36), c(38, 39), c(38, 39), c(39, 40)),
        c(c(41, 42), c(44, 45), c(46, 47), c(47, 48))
      ),
      dim = c(2L, 4L, 3L, 2L)
    )

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
    input <- arr(
      c(
        c(c(1, 2), c(3, 4), c(5, 6), c(7, 8)),
        c(c(9, 10), c(11, 12), c(13, 14), c(15, 16)),
        c(c(17, 18), c(19, 20), c(21, 22), c(23, 24))
      ),
      dim = c(2L, 4L, 3L)
    )

    scatter_indices <- arr(
      c(
        c(c(0, 2), c(1, 0), c(2, 1)),
        c(c(0, 1), c(1, 0), c(0, 9))
      ),
      dim = c(2L, 3L, 2L)
    )

    update <- array(1L, dim = c(2L, 2L, 3L, 2L))

    expected <- arr(
      c(
        c(c(1, 2), c(5, 6), c(7, 8), c(7, 8)),
        c(c(10, 11), c(12, 13), c(14, 15), c(16, 17)),
        c(c(18, 19), c(20, 21), c(21, 22), c(23, 24))
      ),
      dim = c(2L, 4L, 3L)
    )

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
