test_that("If operator works", {
  func <- local_func()
  pred <- hlo_input("pred", "i1", integer())
  x1 <- hlo_input("x1", "f32", integer())
  x2 <- hlo_input("x2", "f32", integer())

  f1 <- hlo_return(hlo_closure(x1)[[1L]])
  f2 <- hlo_return(hlo_closure(x2)[[1L]])

  out <- hlo_if(
    pred = pred,
    true_branch = f1,
    false_branch = f2
  )
  f <- hlo_return(out)
  expect_snapshot(f)

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  expect_equal(
    as_array(pjrt_execute(
      executable,
      pjrt_scalar(TRUE),
      pjrt_scalar(1),
      pjrt_scalar(2)
    )),
    as_array(pjrt_scalar(1))
  )
  expect_equal(
    as_array(pjrt_execute(
      executable,
      pjrt_scalar(FALSE),
      pjrt_scalar(1),
      pjrt_scalar(2)
    )),
    as_array(pjrt_scalar(2))
  )
})

test_that("errors", {
  pred <- vt("pred", integer())
  branch_i32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("i32", 2L))))
  )
  branch_f32 <- Func(
    outputs = FuncOutputs(list(FuncOutput(vt("f32", 2L))))
  )
  branch_2out <- Func(
    outputs = FuncOutputs(list(
      FuncOutput(vt("i32", 2L)),
      FuncOutput(vt("i32", 3L))
    ))
  )
  # different number of outputs
  expect_snapshot(
    infer_types_if(pred, branch_i32, branch_2out),
    error = TRUE
  )
  # output types don't match
  expect_snapshot(
    infer_types_if(pred, branch_i32, branch_f32),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("if requires its branches to agree exactly", {
  # SPEC (C2) is equality, and a widened result is one IREE cannot lower --
  # `scf.if` requires the yielded type to match the region's declared type.
  # So a branch that knows the axis against one that does not is an error, not
  # a widening to `?`.
  expect_error(
    infer_types_if(
      pred = vt("i1", integer()),
      true_branch = fake_func(out = list(vt("f32", 3L))),
      false_branch = fake_func(out = list(vt("f32", N)))
    ),
    class = "ErrorUnequalTypes"
  )
  # Two known but different sizes: also an error, as before.
  expect_error(
    infer_types_if(
      pred = vt("i1", integer()),
      true_branch = fake_func(out = list(vt("f32", 3L))),
      false_branch = fake_func(out = list(vt("f32", 4L)))
    ),
    class = "ErrorUnequalTypes"
  )
  # Branches that agree -- on a known size, or on `?` -- give that type.
  # (C3): the result is a branch's type, not a computed one.
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
  expect_equal(
    repr(
      infer_types_if(
        pred = vt("i1", integer()),
        true_branch = fake_func(out = list(vt("f32", N))),
        false_branch = fake_func(out = list(vt("f32", N)))
      )[[1L]]$type
    ),
    "tensor<?xf32>"
  )
})

test_that("if's branches must not declare inputs", {
  # (C1) `input_types(branches...) = []`. The op passes its branches nothing,
  # so a branch declaring inputs renders `^bb0(%x: ...)` inside `stablehlo.if`
  # and MLIR refuses the region. `case` already caught this.
  pred <- vt("i1", integer())
  out <- list(vt("f32", 3L))
  with_input <- fake_func(inputs = list(vt("f32", 3L)), out = out)
  no_input <- fake_func(out = out)
  expect_error(
    infer_types_if(pred, with_input, no_input),
    "true_branch.*must not have inputs"
  )
  expect_error(
    infer_types_if(pred, no_input, with_input),
    "false_branch.*must not have inputs"
  )
  expect_no_error(infer_types_if(pred, no_input, no_input))
})
