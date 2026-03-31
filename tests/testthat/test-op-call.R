test_that("hlo_call basic repr", {
  mod <- local_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  b <- hlo_input("b", "f32", shape = c(2L, 2L))
  hlo_return(hlo_add(a, b))

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  y <- hlo_input("y", "f32", shape = c(2L, 2L))
  result <- hlo_call(forward, x, y)
  f <- hlo_return(result)
  expect_snapshot(repr(f))
})

test_that("hlo_call with multiple outputs", {
  mod <- local_module()

  helper <- local_func("helper")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  hlo_return(a, a)

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  results <- hlo_call(helper, x, simplify = FALSE)
  f <- hlo_return(results[[1L]], results[[2L]])
  expect_snapshot(repr(f))
})

test_that("hlo_call wrong number of arguments", {
  mod <- local_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  b <- hlo_input("b", "f32", shape = c(2L, 2L))
  hlo_return(hlo_add(a, b))

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  expect_error(
    hlo_call(forward, x),
    "Expected 2 arguments, got 1"
  )
})

test_that("hlo_call wrong argument type", {
  mod <- local_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  hlo_return(a)

  main <- local_func("main")
  x <- hlo_input("x", "i32", shape = c(2L, 2L))
  expect_error(
    hlo_call(forward, x),
    "must match the callee's input type"
  )
})

test_that("hlo_call requires finalized callee", {
  mod <- local_module()

  forward <- local_func("forward")
  # Don't finalize - just leave it unfinished
  unfin <- Func(FuncId("unfin"))

  main <- local_func("main")
  x <- hlo_input("x", "f32")
  expect_error(
    hlo_call(unfin, x),
    "finalized"
  )
})
