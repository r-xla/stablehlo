test_that("basic tests", {
  local_func()
  pred <- hlo_input("pred", "i1", shape = c(2L, 3L, 2L))
  on_true <- hlo_input("on_true", "f32", shape = c(2L, 3L, 2L))
  on_false <- hlo_input("on_false", "f32", shape = c(2L, 3L, 2L))
  y <- hlo_select(
    pred,
    on_true,
    on_false
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  pred <- array(rep(c(TRUE, FALSE), 6), dim = c(2, 3, 2))
  x1 <- array(rep(2, 12), dim = c(2, 3, 2))
  x2 <- array(rep(-2, 12), dim = c(2, 3, 2))
  expected <- x2
  expected[pred] <- x1[pred]

  output <- pjrt_execute(
    exec,
    pjrt_buffer(pred),
    pjrt_buffer(x1),
    pjrt_buffer(x2)
  )
  expect_equal(as_array(output), expected)

  # also works with scalar

  local_func()
  x <- hlo_input("x", "pred")
  z <- hlo_select(x, hlo_tensor(1:2), hlo_tensor(2:3))
  f <- hlo_return(z)
  exec <- pjrt_compile(pjrt_program(src = repr(f)))
  expect_equal(
    pjrt_execute(exec, pjrt_scalar(TRUE)),
    pjrt_buffer(1:2)
  )
  expect_equal(
    pjrt_execute(exec, pjrt_scalar(FALSE)),
    pjrt_buffer(2:3)
  )
})

test_that("select with all i1 types uses generic format", {
  skip_if_not_installed("pjrt")
  local_func()
  pred <- hlo_input("pred", "i1", shape = 2L)
  on_true <- hlo_input("on_true", "i1", shape = 2L)
  on_false <- hlo_input("on_false", "i1", shape = 2L)
  y <- hlo_select(pred, on_true, on_false)
  f <- hlo_return(y)
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)
  output <- pjrt_execute(
    exec,
    pjrt_buffer(c(TRUE, FALSE)),
    pjrt_buffer(c(TRUE, TRUE)),
    pjrt_buffer(c(FALSE, FALSE))
  )
  expect_equal(as.vector(as_array(output)), c(TRUE, FALSE))
})

test_that("errors", {
  # i1 shape mismatch
  expect_snapshot(
    infer_types_select(
      vt("i1", c(3L, 3L)),
      vt("f32", c(2L, 3L)),
      vt("f32", c(2L, 3L))
    ),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("select: unifies its operands, scalar pred still exempt", {
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

test_that("compare and select", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      gt <- hlo_compare(
        a,
        b,
        comparison_direction = "GT",
        compare_type = "FLOAT"
      )
      hlo_select(gt, a, b)
    },
    dyn_shapes = list(N, N),
    runs = list(
      list(shapes = list(4L, 4L), args = list(c(1, 5, 3, 7), c(2, 2, 9, 4))),
      list(shapes = list(2L, 2L), args = list(c(8, 1), c(3, 6)))
    )
  )
})

test_that("select emits parseable MLIR when its operands' shapes differ", {
  skip_if_no_iree_compile()
  # Inference *unifies* the operands, so `on_true`, `on_false` and the result
  # need not share a type -- and the short assembly form names only two of
  # them. Emitting it then produces MLIR that does not parse, which no
  # assertion on the inferred type string would catch.
  #
  # The oracle has to be a compiler. `pjrt::pjrt_program()` validates nothing
  # -- it accepts the literal string "this is not mlir" -- so a test built on
  # it would pass with the bug restored.
  sizes <- c(N, 3L)
  for (p_shape in sizes) {
    for (t_shape in sizes) {
      for (f_shape in sizes) {
        local_func(id = "main")
        src <- repr(hlo_return(hlo_select(
          dyn_input("p", "pred", p_shape),
          dyn_input("t", "f32", t_shape),
          dyn_input("q", "f32", f_shape)
        )))
        res <- iree_compiles(src)
        expect_true(
          res$ok,
          info = paste0(
            "pred=",
            p_shape,
            " on_true=",
            t_shape,
            " on_false=",
            f_shape,
            "\n",
            src,
            "\n",
            res$log
          )
        )
      }
    }
  }
})

test_that("select refines, compiles and runs with a mixed operand set", {
  skip_if_no_refine()
  # The refine-and-run case next to this one uses all-dynamic operands, so all
  # three types coincide and the renderer never leaves its short form.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      gt <- hlo_compare(
        a,
        b,
        comparison_direction = "GT",
        compare_type = "FLOAT"
      )
      hlo_select(gt, a, b)
    },
    dyn_shapes = list(N, 4L),
    runs = list(
      list(shapes = list(4L, 4L), args = list(c(1, 5, 3, 7), c(2, 2, 9, 4)))
    )
  )
})
