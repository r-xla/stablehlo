test_that("elementwise: a dynamic operand meets a static one", {
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", N))
    }),
    "tensor<?xf32>"
  )
  # Refinement: the known side wins, so everything downstream stays static.
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", 3L))
    }),
    "tensor<3xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_multiply(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(2L, N))
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("elementwise: a definite size clash is still an error", {
  local_func()
  expect_error(
    hlo_add(dyn_input("a", "f32", 3L), dyn_input("b", "f32", 4L)),
    "same tensor type"
  )
})

test_that("compare: result is bool over the met shape", {
  expect_equal(
    inferred(function() {
      hlo_compare(
        dyn_input("a", "f32", N),
        dyn_input("b", "f32", 3L),
        comparison_direction = "LT",
        compare_type = "FLOAT"
      )
    }),
    "tensor<3xi1>"
  )
})

test_that("select: meets its operands, scalar pred still exempt", {
  expect_equal(
    inferred(function() {
      hlo_select(
        dyn_input("p", "bool", N),
        dyn_input("t", "f32", N),
        dyn_input("f", "f32", 3L)
      )
    }),
    "tensor<3xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_select(
        hlo_input("p", "bool", shape = integer()),
        dyn_input("t", "f32", N),
        dyn_input("f", "f32", N)
      )
    }),
    "tensor<?xf32>"
  )
})

test_that("reduce: the fold refines and rejects an impossible set", {
  reduce_of <- function(shapes) {
    local_func()
    inputs <- Map(
      function(s, i) dyn_input(paste0("x", i), "f32", s),
      shapes,
      seq_along(shapes)
    )
    inits <- lapply(seq_along(shapes), function(i) {
      hlo_scalar(0, dtype = "f32")
    })
    body <- local_func(id = "")
    args <- lapply(seq_len(2L * length(shapes)), function(i) {
      hlo_input(paste0("b", i), "f32", shape = integer())
    })
    outs <- lapply(seq_along(shapes), function(i) {
      hlo_add(args[[i]], args[[i + length(shapes)]])
    })
    do.call(hlo_return, outs)
    hlo_reduce(inputs, inits, body = body, dimensions = 0L)
  }

  # One dynamic input, one static: the static size wins.
  out <- reduce_of(list(c(N, 3L), c(2L, N)))
  expect_equal(repr(out[[1L]]$value_type$type), "tensor<3xf32>")

  # Every shape may match the first, but the last two cannot match each other.
  # A pairwise check against input 1 would accept this; the fold does not.
  expect_error(reduce_of(list(N, 3L, 4L)), "dimension")
})

test_that("concatenate: off-axis meets, on-axis sums to unknown", {
  expect_equal(
    inferred(function() {
      hlo_concatenate(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(N, 3L)),
        dimension = 0L
      )
    }),
    "tensor<?x3xf32>"
  )
  # A known part does not make the sum known while another part is unknown.
  expect_equal(
    inferred(function() {
      hlo_concatenate(
        dyn_input("a", "f32", c(2L, N)),
        dyn_input("b", "f32", c(N, 4L)),
        dimension = 0L
      )
    }),
    "tensor<?x4xf32>"
  )
  # Off-axis sizes that cannot agree are still refused.
  local_func()
  expect_error(
    hlo_concatenate(
      dyn_input("a", "f32", c(N, 3L)),
      dyn_input("b", "f32", c(N, 4L)),
      dimension = 0L
    )
  )
})

test_that("transpose carries a dynamic axis through the permutation", {
  expect_equal(
    inferred(function() {
      hlo_transpose(dyn_input("a", "f32", c(N, 3L)), permutation = c(1L, 0L))
    }),
    "tensor<3x?xf32>"
  )
})

test_that("broadcast_in_dim defers an unknown operand size", {
  # The operand's axis must be 1 or 3 at run time; we cannot know which, so
  # the check is deferred. The result comes from the static attribute.
  expect_equal(
    inferred(function() {
      hlo_broadcast_in_dim(
        dyn_input("a", "f32", N),
        broadcast_dimensions = 0L,
        shape = 3L
      )
    }),
    "tensor<3xf32>"
  )
  # A known-wrong size is still refused.
  local_func()
  expect_error(
    hlo_broadcast_in_dim(
      dyn_input("a", "f32", 2L),
      broadcast_dimensions = 0L,
      shape = 3L
    ),
    "dimension"
  )
})

test_that("reshape defers the element-count check", {
  expect_equal(
    inferred(function() {
      hlo_reshape(dyn_input("a", "f32", N), shape = c(2L, 3L))
    }),
    "tensor<2x3xf32>"
  )
  local_func()
  expect_error(
    hlo_reshape(dyn_input("a", "f32", 5L), shape = c(2L, 3L)),
    "Size of output"
  )
})

test_that("slice defers the bounds check on an unknown axis", {
  expect_equal(
    inferred(function() {
      hlo_slice(
        dyn_input("a", "f32", c(N, 4L)),
        start_indices = c(0L, 1L),
        limit_indices = c(2L, 3L),
        strides = c(1L, 1L)
      )
    }),
    "tensor<2x2xf32>"
  )
  local_func()
  expect_error(
    hlo_slice(
      dyn_input("a", "f32", c(N, 4L)),
      start_indices = c(0L, 1L),
      limit_indices = c(2L, 9L),
      strides = c(1L, 1L)
    ),
    "limit_indices"
  )
})

test_that("dot_general meets its batch and contracting axes", {
  expect_equal(
    inferred(function() {
      hlo_dot_general(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(3L, 2L)),
        contracting_dims = list(1L, 0L)
      )
    }),
    "tensor<?x2xf32>"
  )
  # A contracting axis known on one side and dynamic on the other is fine.
  expect_equal(
    inferred(function() {
      hlo_dot_general(
        dyn_input("a", "f32", c(4L, N)),
        dyn_input("b", "f32", c(3L, 2L)),
        contracting_dims = list(1L, 0L)
      )
    }),
    "tensor<4x2xf32>"
  )
  # A definite contracting-size clash is still refused.
  local_func()
  expect_error(
    hlo_dot_general(
      dyn_input("a", "f32", c(4L, 5L)),
      dyn_input("b", "f32", c(3L, 2L)),
      contracting_dims = list(1L, 0L)
    ),
    "contracting"
  )
})

test_that("sort refines across its inputs", {
  sort_of <- function(shapes) {
    local_func()
    inputs <- Map(
      function(s, i) dyn_input(paste0("x", i), "f32", s),
      shapes,
      seq_along(shapes)
    )
    cmp <- local_func(id = "")
    l <- hlo_input("l", "f32", shape = integer())
    r <- hlo_input("r", "f32", shape = integer())
    hlo_return(hlo_compare(
      l,
      r,
      comparison_direction = "LT",
      compare_type = "FLOAT"
    ))
    rlang::exec(
      hlo_sort,
      !!!inputs,
      dimension = 0L,
      is_stable = TRUE,
      comparator = cmp
    )
  }
  out <- sort_of(list(N, 3L))
  expect_equal(repr(out[[1L]]$value_type$type), "tensor<3xf32>")
  expect_equal(repr(out[[2L]]$value_type$type), "tensor<3xf32>")
  expect_error(sort_of(list(N, 3L, 4L)), "same shape")
})

test_that("cholesky defers the squareness check", {
  # The trailing two axes must be equal; when either is unknown that is a
  # run-time matter. The result keeps the operand's shape.
  expect_equal(
    inferred(function() {
      hlo_cholesky(dyn_input("a", "f32", c(N, N)), lower = TRUE)
    }),
    "tensor<?x?xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_cholesky(dyn_input("a", "f32", c(3L, N)), lower = TRUE)
    }),
    "tensor<3x?xf32>"
  )
  local_func()
  expect_error(
    hlo_cholesky(dyn_input("a", "f32", c(3L, 4L)), lower = TRUE),
    "dimension"
  )
})

test_that("dynamic_slice and dynamic_update_slice defer their bounds", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_slice(
        dyn_input("a", "f32", N),
        hlo_scalar(0L, dtype = "i32"),
        slice_sizes = 2L
      )
    }),
    "tensor<2xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_dynamic_update_slice(
        dyn_input("a", "f32", N),
        dyn_input("u", "f32", 2L),
        hlo_scalar(0L, dtype = "i32")
      )
    }),
    "tensor<?xf32>"
  )
  # A slice that certainly overruns a known axis is still refused.
  local_func()
  expect_error(
    hlo_dynamic_slice(
      dyn_input("a", "f32", 3L),
      hlo_scalar(0L, dtype = "i32"),
      slice_sizes = 9L
    )
  )
})

test_that("gather takes a dynamic operand, result from slice_sizes", {
  expect_equal(
    inferred(function() {
      hlo_gather(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("i", "i32", c(2L, 1L)),
        gather_dimension_numbers = GatherDimensionNumbers(
          offset_dims = 1L,
          collapsed_slice_dims = 0L,
          start_index_map = 0L,
          index_vector_dim = 1L
        ),
        slice_sizes = c(1L, 3L)
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("scatter folds its inputs and updates", {
  expect_equal(
    inferred(function() {
      hlo_scatter(
        list(dyn_input("a", "f32", c(N, 3L))),
        dyn_input("i", "i32", c(2L, 1L)),
        list(dyn_input("u", "f32", c(2L, 3L))),
        update_computation = add_region(),
        scatter_dimension_numbers = ScatterDimensionNumbers(
          update_window_dims = 1L,
          inserted_window_dims = 0L,
          scatter_dims_to_operand_dims = 0L,
          index_vector_dim = 1L
        ),
        indices_are_sorted = FALSE,
        unique_indices = FALSE
      )
    }),
    "tensor<?x3xf32>"
  )
})

test_that("convolution carries a dynamic batch or spatial axis", {
  conv <- function(lhs_shape) {
    hlo_convolution(
      dyn_input("a", "f32", lhs_shape),
      dyn_input("k", "f32", c(1L, 1L, 2L)),
      dimension_numbers = ConvDimensionNumbers(
        0L,
        1L,
        2L,
        1L,
        0L,
        2L,
        0L,
        1L,
        2L
      ),
      window_strides = 1L,
      padding = matrix(0L, 1L, 2L),
      lhs_dilation = 1L,
      rhs_dilation = 1L
    )
  }
  # A dynamic batch axis stays dynamic; the spatial extent is still computed.
  expect_equal(inferred(function() conv(c(N, 1L, 4L))), "tensor<?x1x3xf32>")
  # A dynamic spatial axis makes the window count unknown.
  expect_equal(inferred(function() conv(c(2L, 1L, N))), "tensor<2x1x?xf32>")
})

test_that("top_k over a dynamic batch axis", {
  local_func()
  out <- hlo_top_k(dyn_input("a", "f32", c(N, 4L)), k = 2L)
  expect_equal(repr(out[[1L]]$value_type$type), "tensor<?x2xf32>")
})

test_that("while carries a dynamic loop state", {
  local_func(id = "main")
  x <- dyn_input("x", "f32", N)
  cond <- local({
    f <- local_func(id = "")
    hlo_input("a", "f32", shape = N)
    hlo_return(hlo_compare(
      hlo_scalar(0, dtype = "f32"),
      hlo_scalar(1, dtype = "f32"),
      comparison_direction = "LT",
      compare_type = "FLOAT"
    ))
    f
  })
  body <- local({
    f <- local_func(id = "")
    a <- hlo_input("a", "f32", shape = N)
    hlo_return(hlo_add(a, a))
    f
  })
  expect_equal(
    repr(hlo_while(x, cond = cond, body = body)$value_type$type),
    "tensor<?xf32>"
  )
})
