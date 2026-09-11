test_that("basic tests", {
  func <- local_func()
  x1 <- hlo_input("x1", "i32", shape = c(3L, 1L))
  x2 <- hlo_input("x2", "i32", shape = c(3L, 2L))
  x3 <- hlo_input("x3", "i32", shape = c(3L, 3L))
  y <- hlo_concatenate(
    x1,
    x2,
    x3,
    dimension = 1L
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  x1 <- array(1L:3L, dim = c(3, 1))
  x2 <- array(1L:6L, dim = c(3, 2))
  x3 <- array(1L:9L, dim = c(3, 3))
  expected <- cbind(x1, x2, x3)

  output <- pjrt_execute(
    exec,
    pjrt_buffer(x1),
    pjrt_buffer(x2),
    pjrt_buffer(x3)
  )
  expect_equal(as_array(output), expected, tolerance = 1e-3)
})

test_that("works with 3D tensors", {
  out <- infer_types_concatenate(
    ValueType(TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 3, 4)))),
    ValueType(TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 1, 4)))),
    dimension = Constant(
      1L,
      TensorType(dtype = as_dtype("i64"), shape = Shape(integer()))
    )
  )[[1L]]$type
  expect_equal(
    out,
    TensorType(dtype = as_dtype("bool"), shape = Shape(c(2, 4, 4)))
  )
})

test_that("errors", {
  # (C3) no inputs
  expect_snapshot(
    infer_types_concatenate(dimension = scnst(0L, "i64")),
    error = TRUE
  )
  # (C1) different data types
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      vt("i32", c(2L, 3L)),
      dimension = scnst(0L, "i64")
    ),
    error = TRUE
  )
  # (C4) dimension out of bounds
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      dimension = scnst(2L, "i64")
    ),
    error = TRUE
  )
  # (C2) non-concat dimension shape mismatch
  expect_snapshot(
    infer_types_concatenate(
      vt("f32", c(2L, 3L)),
      vt("f32", c(2L, 4L)),
      dimension = scnst(0L, "i64")
    ),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

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
    ),
    class = "ErrorConcatenateShapes"
  )
})

test_that("concatenate: the refiner derives the sum we could not", {
  skip_if_no_refine()
  # Our inference says `?` for the concatenated axis; the refiner proves 2n.
  # This is the case where the two disagree in strength but must not disagree
  # in fact.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      hlo_concatenate(a, a, dimension = 0L)
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(3L), args = list(c(1, 2, 3))),
      list(shapes = list(6L), args = list(1:6 + 0))
    )
  )
})

test_that("a dynamic axis survives a chain of ops and reaches the right size", {
  skip_if_no_iree_compile()

  # concatenate is the interesting one: its on-axis size is the *sum*, so a
  # dynamic input gives a dynamic output, and only the runtime knows the
  # result is 2n long.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  sum <- hlo_add(a, b)
  gt <- hlo_compare(sum, a, comparison_direction = "GT", compare_type = "FLOAT")
  sel <- hlo_select(gt, sum, a)
  src <- repr(hlo_return(hlo_concatenate(sel, a, dimension = 0L)))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  for (n in c(2L, 4L)) {
    x <- as.double(seq_len(n))
    y <- rep(1, n)
    got <- iree_run(
      src,
      c(
        sprintf("%dxf32=%s", n, paste(x, collapse = " ")),
        sprintf("%dxf32=%s", n, paste(y, collapse = " "))
      )
    )
    # select(x + y > x, x + y, x) is x + y wherever y > 0, i.e. everywhere.
    expect_equal(got, c(x + y, x), tolerance = 1e-6, info = paste("n =", n))
  }
})

test_that("concatenate compares operand ranks, not just their projections", {
  # Dropping the concatenated axis from a shape that does not have it removes
  # nothing, so the off-axis fold agrees across a rank mismatch. Unguarded,
  # the concatenated axis then reads out of bounds as `NA` and the result
  # carries a `?` that no operand justifies.
  local_func()
  expect_error(
    hlo_concatenate(
      hlo_input("a", "f32", shape = c(2L, 3L)),
      hlo_input("b", "f32", shape = 2L),
      dimension = 1L
    ),
    class = "ErrorConcatenateShapes"
  )
  local_func()
  expect_error(
    hlo_concatenate(
      hlo_input("a", "f32", shape = 3L),
      hlo_scalar(1, dtype = "f32"),
      dimension = 0L
    ),
    class = "ErrorConcatenateShapes"
  )
})
