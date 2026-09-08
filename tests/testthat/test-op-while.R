test_that("simple loop", {
  func <- local_func()

  init_i <- hlo_input("init_i", "i64")
  init_sum <- hlo_input("init_sum", "i64")

  cond <- local_func("cond")
  a0 <- hlo_input("arg0", "i64")
  a1 <- hlo_input("arg1", "i64")
  ten <- hlo_scalar(10L, dtype = "i64")
  c <- hlo_compare(
    a0,
    ten,
    comparison_direction = "LT",
    compare_type = "SIGNED"
  )
  cond <- hlo_return(c)

  body <- local_func("body")
  b0 <- hlo_input("arg0", "i64")
  b1 <- hlo_input("arg1", "i64")
  one <- hlo_scalar(1L, dtype = "i64")
  new_sum <- hlo_add(b1, one)
  new_i <- hlo_add(b0, one)
  body <- hlo_return(new_i, new_sum)

  out <- hlo_while(init_i, init_sum, cond = cond, body = body)
  func <- hlo_return(out[[1L]], out[[2L]])

  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  exec <- pjrt_compile(program)

  res <- pjrt_execute(exec, pjrt_scalar(1L, "i64"), pjrt_scalar(0L, "i64"))
  expect_identical(res[[1L]], pjrt_scalar(10L, "i64"))
  expect_identical(res[[2L]], pjrt_scalar(9L, "i64"))
})

test_that("errors", {
  cond_ok <- local_func("cond_ok")
  x <- hlo_input("x", "i32", 2L)
  pred <- hlo_scalar(TRUE)
  cond_ok <- hlo_return(pred)

  body_ok <- local_func("body_ok")
  x <- hlo_input("x", "i32", 2L)
  body_ok <- hlo_return(x)

  body_wrong <- local_func("body_wrong")
  x <- hlo_input("x", "i32", 2L)
  body_wrong <- hlo_return(hlo_convert(x, "f32"))

  cond_2in <- local_func("cond_2in")
  x <- hlo_input("x", "i32", 2L)
  y <- hlo_input("y", "i32", 2L)
  pred <- hlo_scalar(TRUE)
  cond_2in <- hlo_return(pred)

  # no operands
  expect_snapshot(
    infer_types_while(cond = cond_ok, body = body_ok),
    error = TRUE
  )
  # cond has wrong number of inputs
  expect_snapshot(
    infer_types_while(vt("i32", 2L), cond = cond_2in, body = body_ok),
    error = TRUE
  )
  # body output types don't match
  expect_snapshot(
    infer_types_while(vt("i32", 2L), cond = cond_ok, body = body_wrong),
    error = TRUE
  )
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("while requires its body's type to equal the carried type", {
  dyn <- vt("f32", N)
  # A body that refines `?` to `3` looks sound -- the loop forgets it next
  # iteration -- but SPEC (C2) requires `(T...) -> (T...)`, and `scf.while`
  # requires the yielded type to match the region's input type.
  expect_error(
    infer_types_while(
      dyn,
      cond = fake_func(inputs = list(dyn), out = list(vt("i1", integer()))),
      body = fake_func(out = list(vt("f32", 3L)))
    ),
    class = "ErrorUnequalTypes"
  )
  # And the other direction likewise.
  static <- vt("f32", 3L)
  expect_error(
    infer_types_while(
      static,
      cond = fake_func(inputs = list(static), out = list(vt("i1", integer()))),
      body = fake_func(out = list(vt("f32", N)))
    ),
    class = "ErrorUnequalTypes"
  )
  # A dynamic axis carried consistently is fine, and (C3) returns the carried
  # type.
  expect_equal(
    repr(
      infer_types_while(
        dyn,
        cond = fake_func(inputs = list(dyn), out = list(vt("i1", integer()))),
        body = fake_func(out = list(vt("f32", N)))
      )[[1L]]$type
    ),
    "tensor<?xf32>"
  )
})
