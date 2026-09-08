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

test_that("reshape defers the element-count check when either side is dynamic", {
  # Both known and unequal: still refused.
  local_func()
  expect_error(
    hlo_reshape(dyn_input("a", "f32", c(2L, 3L)), shape = c(4L, 2L)),
    "Size of output must equal"
  )
  # Dynamic operand: whether the counts match is a run-time question.
  expect_equal(
    inferred(function() {
      hlo_reshape(dyn_input("a", "f32", c(N, 3L)), shape = c(6L, 1L))
    }),
    "tensor<6x1xf32>"
  )
  # A dynamic *result* axis is equally acceptable.
  expect_equal(
    inferred(function() {
      hlo_reshape(dyn_input("a", "f32", c(2L, 3L)), shape = c(N, 2L))
    }),
    "tensor<?x2xf32>"
  )
})

test_that("broadcast_in_dim leaves a dynamic axis to the runtime", {
  # A dynamic operand axis may be 1 (stretch) or 4 (pass through) at run time.
  expect_equal(
    inferred(function() {
      hlo_broadcast_in_dim(
        dyn_input("a", "f32", N),
        shape = c(2L, 4L),
        broadcast_dimensions = 1L
      )
    }),
    "tensor<2x4xf32>"
  )
  # Known, not 1, and not equal: certainly wrong.
  local_func()
  expect_error(
    hlo_broadcast_in_dim(
      dyn_input("a", "f32", 3L),
      shape = c(2L, 4L),
      broadcast_dimensions = 1L
    ),
    class = "ErrorDimSizeMismatch"
  )
})

test_that("clamp accepts a dynamic bound and refines from it", {
  expect_equal(
    inferred(function() {
      hlo_clamp(
        dyn_input("lo", "f32", N),
        dyn_input("x", "f32", 3L),
        dyn_input("hi", "f32", N)
      )
    }),
    "tensor<3xf32>"
  )
  # The operand is dynamic but a bound is not, so the result is static.
  expect_equal(
    inferred(function() {
      hlo_clamp(
        dyn_input("lo", "f32", 4L),
        dyn_input("x", "f32", N),
        dyn_input("hi", "f32", integer())
      )
    }),
    "tensor<4xf32>"
  )
  local_func()
  expect_error(
    hlo_clamp(
      dyn_input("lo", "f32", 4L),
      dyn_input("x", "f32", 3L),
      dyn_input("hi", "f32", integer())
    ),
    "same shape as"
  )
})

test_that("cholesky pins a dynamic trailing axis against its partner", {
  expect_equal(
    inferred(function() {
      hlo_cholesky(dyn_input("a", "f32", c(N, 4L)), lower = TRUE)
    }),
    "tensor<4x4xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_cholesky(dyn_input("a", "f32", c(N, N)), lower = TRUE)
    }),
    "tensor<?x?xf32>"
  )
  local_func()
  expect_error(
    hlo_cholesky(dyn_input("a", "f32", c(3L, 4L)), lower = TRUE),
    "must be symmetric"
  )
})

test_that("sort folds its inputs' shapes instead of comparing pairwise", {
  local_func("comparator")
  cmp <- hlo_compare(
    hlo_input("x", "i32"),
    hlo_input("y", "i32"),
    comparison_direction = "LT",
    compare_type = "SIGNED"
  )
  comparator <- hlo_return(cmp)

  # (3, ?, 4) must be refused: a pairwise check against the first input would
  # accept it, because each of the others may match `?`.
  expect_error(
    infer_types_sort(
      vt("i32", 3L),
      vt("i32", N),
      vt("i32", 4L),
      dimension = scnst(0L, "i64"),
      is_stable = scnst(TRUE, "pred"),
      comparator = comparator
    ),
    "same shape"
  )
  # A set that can agree infers the refined shape.
  expect_equal(
    repr(
      infer_types_sort(
        vt("i32", N),
        vt("i32", 4L),
        dimension = scnst(0L, "i64"),
        is_stable = scnst(TRUE, "pred"),
        comparator = comparator
      )[[1L]]$type
    ),
    "tensor<4xi32>"
  )
})

test_that("dot_general defers contracted sizes and meets batch sizes", {
  # A dynamic contracted axis: the sizes may agree at run time, and neither
  # reaches the result.
  expect_equal(
    inferred(function() {
      hlo_dot_general(
        dyn_input("a", "f32", c(2L, N)),
        dyn_input("b", "f32", c(3L, 4L)),
        contracting_dims = list(1L, 0L)
      )
    }),
    "tensor<2x4xf32>"
  )
  # Both known and different: still refused.
  local_func()
  expect_error(
    hlo_dot_general(
      dyn_input("a", "f32", c(2L, 5L)),
      dyn_input("b", "f32", c(3L, 4L)),
      contracting_dims = list(1L, 0L)
    ),
    class = "ErrorDotGeneralDimMismatch"
  )
  # A batch axis *does* reach the result, so it is refined rather than copied
  # from the lhs: `?` on the left meets `5` on the right.
  expect_equal(
    inferred(function() {
      hlo_dot_general(
        dyn_input("a", "f32", c(N, 2L, 3L)),
        dyn_input("b", "f32", c(5L, 3L, 4L)),
        contracting_dims = list(2L, 1L),
        batching_dims = list(0L, 0L)
      )
    }),
    "tensor<5x2x4xf32>"
  )
})

test_that("slice and dynamic_slice defer their bounds against a dynamic axis", {
  # limit 4 on an axis of unknown size: legal if the operand is long enough.
  expect_equal(
    inferred(function() {
      hlo_slice(
        dyn_input("a", "f32", N),
        start_indices = 0L,
        limit_indices = 4L,
        strides = 1L
      )
    }),
    "tensor<4xf32>"
  )
  # Known and too short: refused.
  local_func()
  expect_error(
    hlo_slice(
      dyn_input("a", "f32", 3L),
      start_indices = 0L,
      limit_indices = 4L,
      strides = 1L
    ),
    class = "ErrorIndexOutOfBounds"
  )
})

test_that("top_k defers k against a dynamic last axis", {
  # The result's last axis is k, which is known, so the result stays static.
  expect_equal(
    inferred(function() {
      hlo_top_k(dyn_input("a", "f32", c(2L, N)), k = 3L)[[1L]]
    }),
    "tensor<2x3xf32>"
  )
  local_func()
  expect_error(
    hlo_top_k(dyn_input("a", "f32", c(2L, 2L)), k = 3L),
    "must not exceed"
  )
})

test_that("pad and transpose carry a dynamic axis through arithmetic", {
  # NA propagates through pad's result arithmetic, so a padded dynamic axis
  # stays dynamic while the static one is computed.
  expect_equal(
    inferred(function() {
      hlo_pad(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("v", "f32", integer()),
        edge_padding_low = c(1L, 1L),
        edge_padding_high = c(1L, 1L),
        interior_padding = c(0L, 0L)
      )
    }),
    "tensor<?x5xf32>"
  )
})

test_that("if/case join their branches rather than meeting them", {
  # One branch knows the axis, the other does not: only one branch runs, so
  # the result cannot claim the known size. It widens to `?`.
  expect_equal(
    repr(
      infer_types_if(
        pred = vt("i1", integer()),
        true_branch = fake_func(out = list(vt("f32", 3L))),
        false_branch = fake_func(out = list(vt("f32", N)))
      )[[1L]]$type
    ),
    "tensor<?xf32>"
  )
  # Both branches agree on a known size: it survives.
  expect_equal(
    repr(
      infer_types_if(
        pred = vt("i1", integer()),
        true_branch = fake_func(out = list(vt("f32", 3L))),
        false_branch = fake_func(out = list(vt("f32", 3L)))
      )[[1L]]$type
    ),
    "tensor<3xf32>"
  )
  # Both known and different is still an error, not a widening: the branches
  # are required to agree, and turning that into `?` would hide a bug.
  expect_error(
    infer_types_if(
      pred = vt("i1", integer()),
      true_branch = fake_func(out = list(vt("f32", 3L))),
      false_branch = fake_func(out = list(vt("f32", 4L)))
    ),
    class = "ErrorUnequalTypes"
  )
})

test_that("while accepts a body that refines, but not one that forgets", {
  carried <- vt("f32", N)
  # Body refines `?` to `3`: fine. The loop still carries `?`, because after
  # zero iterations the result is the input.
  expect_equal(
    repr(
      infer_types_while(
        carried,
        cond = fake_func(
          inputs = list(carried),
          out = list(vt("i1", integer()))
        ),
        body = fake_func(out = list(vt("f32", 3L)))
      )[[1L]]$type
    ),
    "tensor<?xf32>"
  )
  # The other direction: a loop declaring a known size whose body only
  # promises `?` would be claiming a size no iteration guarantees.
  static <- vt("f32", 3L)
  expect_error(
    infer_types_while(
      static,
      cond = fake_func(inputs = list(static), out = list(vt("i1", integer()))),
      body = fake_func(out = list(vt("f32", N)))
    ),
    class = "ErrorUnequalTypes"
  )
})

test_that("one dynamic executable runs at more than one size", {
  skip_if_no_iree_runtime()

  # The whole point of the feature, as one assertion: compile *once* against
  # `tensor<?xf32>`, then run the same executable at two different sizes.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  src <- repr(hlo_return(hlo_add(a, b)))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(src))

  for (n in c(3L, 5L, 1L)) {
    x <- as.double(seq_len(n))
    out <- pjrt::pjrt_execute(
      exec,
      pjrt::pjrt_buffer(x, dtype = "f32"),
      pjrt::pjrt_buffer(x, dtype = "f32")
    )
    expect_equal(
      as.vector(tengen::as_array(out)),
      x + x,
      tolerance = 1e-6,
      info = paste("n =", n)
    )
  }
})

test_that("a refined type still runs, and refinement did not lie", {
  skip_if_no_iree_runtime()

  # `add(tensor<?xf32>, tensor<3xf32>)` infers `tensor<3xf32>`: inference
  # claims the dynamic side must be 3. Check the claim is one the runtime
  # agrees with, rather than trusting the inference that produced it.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", 3L)
  out <- hlo_add(a, b)
  expect_equal(repr(out$value_type$type), "tensor<3xf32>")

  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(hlo_return(out))))
  res <- pjrt::pjrt_execute(
    exec,
    pjrt::pjrt_buffer(c(1, 2, 3), dtype = "f32"),
    pjrt::pjrt_buffer(c(10, 20, 30), dtype = "f32")
  )
  expect_equal(
    as.vector(tengen::as_array(res)),
    c(11, 22, 33),
    tolerance = 1e-6
  )
})

test_that("a dynamic axis survives a chain of ops and reaches the right size", {
  skip_if_no_iree_runtime()

  # concatenate is the interesting one: its on-axis size is the *sum*, so a
  # dynamic input gives a dynamic output, and only the runtime knows the
  # result is 2n long.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  cat2 <- hlo_concatenate(sel, a, dimension = 0L)
  src <- repr(hlo_return(cat2))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  exec <- pjrt::pjrt_compile(pjrt::pjrt_program(src))

  for (n in c(2L, 4L)) {
    x <- as.double(seq_len(n))
    y <- rep(1, n)
    out <- pjrt::pjrt_execute(
      exec,
      pjrt::pjrt_buffer(x, dtype = "f32"),
      pjrt::pjrt_buffer(y, dtype = "f32")
    )
    got <- as.vector(tengen::as_array(out))
    # select(x + y > x, x + y, x) is x + y wherever y > 0, i.e. everywhere.
    expect_equal(got, c(x + y, x), tolerance = 1e-6, info = paste("n =", n))
    expect_length(got, 2L * n)
  }
})

test_that("the inferred dynamic types compile with IREE", {
  skip_if_no_iree_compile()

  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  cat2 <- hlo_concatenate(sel, a, dimension = 0L)
  src <- repr(hlo_return(cat2))

  expect_match(src, "tensor<?xf32>", fixed = TRUE)
  res <- iree_compiles(src)
  expect_true(res$ok, info = res$log)
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
