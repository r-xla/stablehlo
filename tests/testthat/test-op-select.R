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
