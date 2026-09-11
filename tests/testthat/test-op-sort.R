test_that("basic tests", {
  local_func()
  a1 <- hlo_input("arg1", "i32", shape = integer())
  a2 <- hlo_input("arg2", "i32", shape = integer())
  a3 <- hlo_input("arg3", "i32", shape = integer())
  a4 <- hlo_input("arg4", "i32", shape = integer())
  comp <- hlo_compare(
    a1,
    a2,
    comparison_direction = "GT",
    compare_type = "SIGNED"
  )
  comp.func <- hlo_return(comp)

  f <- list()
  for (i in seq_len(2)) {
    func <- local_func()
    x1 <- hlo_input("x1", "i32", shape = c(2L, 3L))
    x2 <- hlo_input("x2", "i32", shape = c(2L, 3L))
    y <- hlo_sort(
      x1,
      x2,
      dimension = 0L,
      is_stable = TRUE,
      comparator = comp.func
    )
    f[[i]] <- hlo_return(y[[i]], func = func)
  }

  expect_snapshot(lapply(f, repr))

  skip_if_not_installed("pjrt")
  program <- lapply(f, \(x) pjrt_program(repr(x)))
  exec <- lapply(program, \(x) pjrt_compile(x))

  x1 <- array(1L:6L * 10L, dim = c(2, 3))
  x2 <- array(1L:6L * 10L + 1L, dim = c(2, 3))

  expected <- list(x1[c(2, 1), ], x2[c(2, 1), ])

  output <- lapply(exec, \(x) {
    pjrt_execute(
      x,
      pjrt_buffer(x1),
      pjrt_buffer(x2)
    )
  })
  expect_equal(lapply(output, as_array), expected, tolerance = 1e-3)
})

test_that("errors", {
  comparator <- local_func("comparator")
  x <- hlo_input("x", "i32")
  y <- hlo_input("y", "i32")
  cmp <- hlo_compare(x, y, comparison_direction = "LT", compare_type = "SIGNED")
  comparator <- hlo_return(cmp)
  # (C1) no inputs
  expect_snapshot(
    infer_types_sort(
      dimension = scnst(0L, "i64"),
      is_stable = scnst(TRUE, "pred"),
      comparator = comparator
    ),
    error = TRUE
  )
  # (C3) different shapes
  expect_snapshot(
    infer_types_sort(
      vt("i32", c(2L, 3L)),
      vt("i32", c(3L, 3L)),
      dimension = scnst(0L, "i64"),
      is_stable = scnst(TRUE, "pred"),
      comparator = comparator
    ),
    error = TRUE
  )
  # (C4) dimension out of bounds
  expect_snapshot(
    infer_types_sort(
      vt("i32", c(2L, 3L)),
      dimension = scnst(5L, "i64"),
      is_stable = scnst(TRUE, "pred"),
      comparator = comparator
    ),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

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

test_that("sort over a dynamic axis", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      hlo_sort(
        dyn_input("x", "f32", shapes[[1L]]),
        dimension = 0L,
        is_stable = TRUE,
        comparator = lt_region()
      )[[1L]]
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(4L), args = list(c(3, 1, 4, 2))),
      list(shapes = list(6L), args = list(c(9, 2, 7, 1, 8, 3)))
    )
  )
})
