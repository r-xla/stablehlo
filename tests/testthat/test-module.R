test_that("Module repr with multiple functions", {
  mod <- local_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  b <- hlo_input("b", "f32", shape = c(2L, 2L))
  hlo_return(hlo_add(a, b))

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  y <- hlo_input("y", "f32", shape = c(2L, 2L))
  result <- hlo_call(forward, x, y)
  hlo_return(result)

  expect_snapshot(repr(mod))
})

test_that("Module is finalized when main is returned", {
  mod <- hlo_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  hlo_return(a)

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  result <- hlo_call(forward, x)
  hlo_return(result)

  expect_error(.current_module(), "No module is currently being built")
  expect_length(mod$funcs, 2L)
})

test_that("local_module restores previous module", {
  mod1 <- local_module()
  g <- function() {
    mod2 <- local_module()
    expect_identical(.current_module(), mod2)
  }
  g()
  expect_identical(.current_module(), mod1)
})

test_that("hlo_module discards previous module", {
  mod1 <- hlo_module()
  mod2 <- hlo_module()
  expect_identical(.current_module(), mod2)
  globals[["CURRENT_MODULE"]] <- NULL
})

test_that("functions auto-register into module", {
  mod <- local_module()

  f1 <- local_func("f1")
  hlo_return(hlo_input("x", "f32"))

  f2 <- local_func("f2")
  hlo_return(hlo_input("y", "f32"))

  expect_length(mod$funcs, 2L)
  expect_equal(mod$funcs[[1]]$id, FuncId("f1"))
  expect_equal(mod$funcs[[2]]$id, FuncId("f2"))
})

test_that("Module without module context still works for Func", {
  globals[["CURRENT_MODULE"]] <- NULL
  f <- local_func("test")
  x <- hlo_input("x", "f32")
  hlo_return(x)
  expect_true(test_class(f, "Func"))
})

test_that("Module with single function", {
  mod <- local_module()
  f <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  hlo_return(x)
  expect_snapshot(repr(mod))
})

test_that("Module rejects duplicate function names", {
  mod <- local_module()

  f1 <- local_func("forward")
  hlo_return(hlo_input("x", "f32"))

  expect_error(
    local_func("forward"),
    "already exists in the module"
  )
})

test_that("PJRT execution with module", {
  skip_if_not_installed("pjrt")

  mod <- local_module()

  forward <- local_func("forward")
  a <- hlo_input("a", "f32", shape = c(2L, 2L))
  b <- hlo_input("b", "f32", shape = c(2L, 2L))
  hlo_return(hlo_add(a, b))

  main <- local_func("main")
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  y <- hlo_input("y", "f32", shape = c(2L, 2L))
  result <- hlo_call(forward, x, y)
  hlo_return(result)

  program <- pjrt::pjrt_program(repr(mod))
  exec <- pjrt::pjrt_compile(program)
  x_buf <- pjrt::pjrt_buffer(1:4, shape = c(2, 2), dtype = "f32")
  y_buf <- pjrt::pjrt_buffer(5:8, shape = c(2, 2), dtype = "f32")
  out <- pjrt::pjrt_execute(exec, x_buf, y_buf)
  expect_equal(
    out,
    pjrt::pjrt_buffer(c(6L, 8L, 10L, 12L), shape = c(2, 2), dtype = "f32")
  )
})
