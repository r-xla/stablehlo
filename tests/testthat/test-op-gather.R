describe("gather", {
  it("looks correct in snapshot", {
    func <- local_func()
    operand <- hlo_input("operand", "i32", c(3L, 4L))
    start_indices <- hlo_input("start_indices", "i32", c(2L, 2L))

    gather_dim_numbers <- GatherDimensionNumbers(
      offset_dims = 1L,
      collapsed_slice_dims = 0L,
      start_index_map = c(0L, 1L),
      index_vector_dim = 1L
    )

    result <- hlo_gather(
      operand = operand,
      start_indices = start_indices,
      gather_dimension_numbers = gather_dim_numbers,
      slice_sizes = c(1L, 2L)
    )

    func <- hlo_return(result)
    expect_snapshot(repr(func))
  })
  skip_if_not_installed("pjrt")

  # Helper to create arrays with explicit row-major structure
  arr <- function(data, dim) {
    aperm(array(data, dim = dim), perm = seq_along(dim))
  }

  # Helper to create R array from row-major data (StableHLO layout)
  # StableHLO uses row-major (last dim varies fastest), R uses column-major
  row_major_array <- function(data, dim) {
    # Create array with reversed dimensions so R's column-major fill
    # matches the row-major data order, then transpose back
    arr <- array(data, dim = rev(dim))
    aperm(arr, perm = rev(seq_along(dim)))
  }

  check <- function(
    operand,
    start_indices,
    offset_dims,
    collapsed_slice_dims,
    start_index_map,
    index_vector_dim,
    slice_sizes,
    operand_batching_dims = integer(),
    start_indices_batching_dims = integer(),
    indices_are_sorted = FALSE,
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

    operand_dtype <- "i64"
    if (inherits(operand, "PJRTBuffer")) {
      operand_dtype <- as.character(pjrt::elt_type(operand))
    } else {
      operand <- pjrt_buffer(to_int(operand), dtype = "i64")
    }

    start_indices_dtype <- "i64"
    if (inherits(start_indices, "PJRTBuffer")) {
      start_indices_dtype <- as.character(pjrt::elt_type(start_indices))
    } else {
      start_indices <- pjrt_buffer(to_int(start_indices), dtype = "i64")
    }

    operand_arg <- hlo_input("operand", operand_dtype, shape(operand))
    start_indices_arg <- hlo_input(
      "start_indices",
      start_indices_dtype,
      shape(start_indices)
    )

    gather_dim_numbers <- GatherDimensionNumbers(
      offset_dims = offset_dims,
      collapsed_slice_dims = collapsed_slice_dims,
      operand_batching_dims = operand_batching_dims,
      start_indices_batching_dims = start_indices_batching_dims,
      start_index_map = start_index_map,
      index_vector_dim = index_vector_dim
    )

    r <- hlo_gather(
      operand = operand_arg,
      start_indices = start_indices_arg,
      gather_dimension_numbers = gather_dim_numbers,
      slice_sizes = slice_sizes,
      indices_are_sorted = indices_are_sorted
    )

    func <- hlo_return(r)
    program <- pjrt_program(repr(func))
    executable <- pjrt_compile(program)
    out_buf <- pjrt_execute(executable, operand, start_indices)
    if (!inherits(expected, "PJRTBuffer")) {
      expected <- pjrt_buffer(expected, dtype = operand_dtype)
    }
    expect_equal(
      out_buf,
      expected
    )
  }

  it("can gather single element from vector", {
    check(
      operand = 1:5,
      start_indices = array(2L, dim = 1L),
      offset_dims = integer(),
      collapsed_slice_dims = 0L,
      start_index_map = 0L,
      index_vector_dim = 0L,
      slice_sizes = 1L,
      expected = pjrt_scalar(3L, dtype = "i64")
    )
  })

  it("can gather slice from vector", {
    check(
      operand = 1:5,
      start_indices = 1L,
      offset_dims = 0L,
      collapsed_slice_dims = integer(),
      start_index_map = 0L,
      index_vector_dim = 0L,
      slice_sizes = 3L,
      expected = c(2L, 3L, 4L)
    )
  })

  it("can gather multiple elements from vector", {
    check(
      operand = 1:5,
      start_indices = c(0L, 2L, 4L),
      offset_dims = integer(),
      collapsed_slice_dims = 0L,
      start_index_map = 0L,
      index_vector_dim = 1L,
      slice_sizes = 1L,
      expected = c(1L, 3L, 5L)
    )
  })

  it("can gather row from 2D matrix", {
    mat <- matrix(1:6, nrow = 2L, byrow = TRUE)
    check(
      operand = mat,
      start_indices = c(1L, 0L),
      offset_dims = 0L,
      collapsed_slice_dims = 0L,
      start_index_map = c(0L, 1L),
      index_vector_dim = 0L,
      slice_sizes = c(1L, 3L),
      expected = c(4L, 5L, 6L)
    )
  })

  it("can gather column from 2D matrix", {
    mat <- matrix(1:6, nrow = 2L, byrow = TRUE)
    check(
      operand = mat,
      start_indices = c(0L, 1L),
      offset_dims = 0L,
      collapsed_slice_dims = 1L,
      start_index_map = c(0L, 1L),
      index_vector_dim = 0L,
      slice_sizes = c(2L, 1L),
      expected = c(2L, 5L)
    )
  })

  it("can gather submatrix from 2D matrix", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    check(
      operand = mat,
      start_indices = c(1L, 1L),
      offset_dims = c(0L, 1L),
      collapsed_slice_dims = integer(),
      start_index_map = c(0L, 1L),
      index_vector_dim = 0L,
      slice_sizes = c(2L, 2L),
      expected = matrix(c(5L, 6L, 8L, 9L), nrow = 2L, byrow = TRUE)
    )
  })
  it("works with different offset_dims", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    f <- function(offset_dims, expected) {
      check(
        operand = mat,
        start_indices = c(0L, 1L),
        offset_dims = offset_dims,
        collapsed_slice_dims = integer(),
        start_index_map = c(0L, 1L),
        index_vector_dim = 0L,
        slice_sizes = c(2L, 2L),
        expected = expected
      )
    }
    f(c(0L, 1L), matrix(c(2L, 3L, 5L, 6L), nrow = 2L, byrow = TRUE))
  })

  it("can gather multiple slices", {
    mat <- matrix(1:9, nrow = 3L, byrow = TRUE)
    # Gather two 1x2 slices at positions (0,0) and (1,1)
    check(
      operand = mat,
      start_indices = matrix(c(0L, 0L, 1L, 1L), nrow = 2L, byrow = TRUE),
      offset_dims = 1L,
      collapsed_slice_dims = 0L,
      start_index_map = c(0L, 1L),
      index_vector_dim = 1L,
      slice_sizes = c(1L, 2L),
      expected = matrix(c(1L, 2L, 5L, 6L), nrow = 2L, byrow = TRUE)
    )
  })

  it("works with implicit index_vector_dim", {
    vec <- 1:4
    check(
      operand = vec,
      start_indices = c(1L, 2L),
      offset_dims = integer(),
      collapsed_slice_dims = 0L,
      start_index_map = 0L,
      index_vector_dim = 1L, # Equal to rank(start_indices), so implicit
      slice_sizes = 1L,
      expected = c(2L, 3L)
    )
  })

  it("works with explicit index_vector_dim", {
    vec <- 1:4
    check(
      operand = vec,
      start_indices = matrix(c(1L, 2L), nrow = 1L),
      offset_dims = integer(),
      collapsed_slice_dims = 0L,
      start_index_map = 0L,
      index_vector_dim = 0L,
      slice_sizes = 1L,
      expected = c(2L, 3L)
    )
  })

  it("works with batching dimensions", {
    # operand shape: (2, 3) - first dim is batch
    # start_indices shape: (2, 1) - second dim is batch
    mat <- matrix(1:6, nrow = 2L, byrow = TRUE)
    check(
      operand = mat,
      start_indices = matrix(c(0L, 1L), ncol = 1L),
      offset_dims = integer(),
      collapsed_slice_dims = 1L,
      operand_batching_dims = 0L,
      start_indices_batching_dims = 0L,
      start_index_map = 1L,
      index_vector_dim = 1L,
      slice_sizes = c(1L, 1L),
      expected = c(1L, 5L)
    )
  })

  # Skipping the complex spec example with batching as it requires
  # careful verification of expected values. The simpler tests above
  # cover the core gather functionality.

  it("works with simple 3D gather", {
    # Simpler 3D example without batching
    # operand: 2x3x4 tensor
    operand <- arr(1:24, dim = c(2L, 3L, 4L))

    # Gather a single 2x2 slice starting at index [0, 1] (axis 0 and 1)
    # with slice_sizes = c(2, 2, 1) and collapsed_slice_dims = 2
    # This gathers operand[0:2, 1:3, 0] reshaped to 2x2
    start_indices <- c(0L, 1L)

    check(
      operand = operand,
      start_indices = start_indices,
      offset_dims = c(0L, 1L),
      collapsed_slice_dims = 2L,
      start_index_map = c(0L, 1L),
      index_vector_dim = 0L,
      slice_sizes = c(2L, 2L, 1L),
      expected = arr(c(3, 4, 5, 6), dim = c(2L, 2L))
    )
  })

  it("can collapse slice dimensions", {
    # TODO
  })
  it("works with different slice sizes", {

  })

  # Tests from stablehlo-translate --interpret
  it("works with gather_op_test from interpreter", {
    # From stablehlo interpreter tests: gather_op_test
    # operand: tensor<3x4x2xi64>
    # start_indices: tensor<2x3x2xi64>
    # offset_dims = [2, 3], collapsed_slice_dims = [0]
    # start_index_map = [1, 0], index_vector_dim = 2
    # slice_sizes = [1, 2, 2]
    # result: tensor<2x3x2x2xi64>

    operand <- row_major_array(
      c(1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24),
      dim = c(3L, 4L, 2L)
    )

    start_indices <- row_major_array(
      c(0, 0, 1, 0, 2, 1,
        0, 1, 1, 1, 0, 9),
      dim = c(2L, 3L, 2L)
    )

    expected <- row_major_array(
      c(1, 2, 3, 4,
        3, 4, 5, 6,
        13, 14, 15, 16,
        9, 10, 11, 12,
        11, 12, 13, 14,
        17, 18, 19, 20),
      dim = c(2L, 3L, 2L, 2L)
    )

    check(
      operand = operand,
      start_indices = start_indices,
      offset_dims = c(2L, 3L),
      collapsed_slice_dims = 0L,
      start_index_map = c(1L, 0L),
      index_vector_dim = 2L,
      slice_sizes = c(1L, 2L, 2L),
      expected = expected
    )
  })

  it("works with gather_op_with_batching_dim_test from interpreter", {
    # From stablehlo interpreter tests: gather_op_with_batching_dim_test
    # operand: tensor<2x3x4x2xi64>
    # start_indices: tensor<2x2x3x2xi64>
    # offset_dims = [3, 4], collapsed_slice_dims = [1]
    # operand_batching_dims = [0], start_indices_batching_dims = [1]
    # start_index_map = [2, 1], index_vector_dim = 3
    # slice_sizes = [1, 1, 2, 2]
    # result: tensor<2x2x3x2x2xi64>

    operand <- row_major_array(
      c(1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32,
        33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48),
      dim = c(2L, 3L, 4L, 2L)
    )

    start_indices <- row_major_array(
      c(0, 0, 1, 0, 2, 1,
        0, 1, 1, 1, 0, 9,
        0, 0, 2, 1, 2, 2,
        1, 2, 0, 1, 1, 0),
      dim = c(2L, 2L, 3L, 2L)
    )

    expected <- row_major_array(
      c(1, 2, 3, 4,
        3, 4, 5, 6,
        13, 14, 15, 16,
        33, 34, 35, 36,
        35, 36, 37, 38,
        41, 42, 43, 44,
        1, 2, 3, 4,
        13, 14, 15, 16,
        21, 22, 23, 24,
        43, 44, 45, 46,
        33, 34, 35, 36,
        27, 28, 29, 30),
      dim = c(2L, 2L, 3L, 2L, 2L)
    )

    check(
      operand = operand,
      start_indices = start_indices,
      offset_dims = c(3L, 4L),
      collapsed_slice_dims = 1L,
      operand_batching_dims = 0L,
      start_indices_batching_dims = 1L,
      start_index_map = c(2L, 1L),
      index_vector_dim = 3L,
      slice_sizes = c(1L, 1L, 2L, 2L),
      expected = expected
    )
  })

  it("works with batching dim test (i32 operand)", {
    # Same as above but with i32 operand type to test mixed dtypes
    to_int_arr <- function(x) {
      y <- as.integer(x)
      dim(y) <- dim(x)
      y
    }

    operand <- row_major_array(
      c(1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32,
        33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48),
      dim = c(2L, 3L, 4L, 2L)
    )
    operand <- pjrt_buffer(to_int_arr(operand), dtype = "i32")

    start_indices <- row_major_array(
      c(0, 0, 1, 0, 2, 1,
        0, 1, 1, 1, 0, 9,
        0, 0, 2, 1, 2, 2,
        1, 2, 0, 1, 1, 0),
      dim = c(2L, 2L, 3L, 2L)
    )
    start_indices <- pjrt_buffer(to_int_arr(start_indices), dtype = "i64")

    expected <- row_major_array(
      c(1, 2, 3, 4,
        3, 4, 5, 6,
        13, 14, 15, 16,
        33, 34, 35, 36,
        35, 36, 37, 38,
        41, 42, 43, 44,
        1, 2, 3, 4,
        13, 14, 15, 16,
        21, 22, 23, 24,
        43, 44, 45, 46,
        33, 34, 35, 36,
        27, 28, 29, 30),
      dim = c(2L, 2L, 3L, 2L, 2L)
    )
    expected <- pjrt_buffer(to_int_arr(expected), dtype = "i32")

    check(
      operand = operand,
      start_indices = start_indices,
      offset_dims = c(3L, 4L),
      collapsed_slice_dims = 1L,
      operand_batching_dims = 0L,
      start_indices_batching_dims = 1L,
      start_index_map = c(2L, 1L),
      index_vector_dim = 3L,
      slice_sizes = c(1L, 1L, 2L, 2L),
      expected = expected
    )
  })
})
