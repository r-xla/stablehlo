test_that("matmul", {
  func <- local_func()
  lhs <- hlo_input("lhs", "f32", shape = c(5, 4))
  rhs <- hlo_input("rhs", "f32", shape = c(4, 3))
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(1L, 0L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")

  pjrt_program <- pjrt_program(repr(f))
  exec <- pjrt_compile(pjrt_program)

  expect_equal(
    pjrt_execute(
      exec,
      pjrt_buffer(array(as.double(1:20), dim = c(5, 4))),
      pjrt_buffer(array(as.double(1:12), dim = c(4, 3)))
    ),
    pjrt_buffer(
      array(as.double(1:20), dim = c(5, 4)) %*%
        array(as.double(1:12), dim = c(4, 3))
    )
  )
})

test_that("batching_dims", {
  skip_if_not_installed("pjrt")
  func <- local_func()
  lhs <- hlo_input("lhs", "f32", shape = c(1, 5, 4))
  rhs <- hlo_input("rhs", "f32", shape = c(4, 3, 1L))
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(2L, 0L),
    batching_dims = list(0L, 2L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  x1 <- array(as.double(1:20), dim = c(1, 5, 4))
  x2 <- array(as.double(1:12), dim = c(4, 3, 1L))
  x1b <- pjrt_buffer(x1)
  x2b <- pjrt_buffer(x2)

  pjrt_program <- pjrt_program(repr(f))
  exec <- pjrt_compile(pjrt_program)
  dim(x1) <- c(5, 4)
  dim(x2) <- c(4, 3)
  x <- x1 %*% x2
  dim(x) <- c(1, 5, 3)

  expect_equal(
    pjrt_execute(exec, x1b, x2b),
    pjrt_buffer(x)
  )
})

test_that("dot product", {
  skip_if_not_installed("pjrt")
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = 10)
  rhs <- hlo_input("rhs", "f32", shape = 10)
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(0L, 0L)
  )
  f <- hlo_return(z)

  pjrt_program <- pjrt::pjrt_program(repr(f))
  exec <- pjrt::pjrt_compile(pjrt_program)

  x1 <- pjrt::pjrt_buffer(1:10, "f32")
  x2 <- pjrt::pjrt_buffer(2:11, "f32")

  out <- pjrt::pjrt_execute(exec, x1, x2)
  expect_equal(out, pjrt::pjrt_scalar(sum(1:10 * 2:11), "f32"))
})

test_that("no contracting dims", {
  skip_if_not_installed("pjrt")
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = 10L)
  rhs <- hlo_input("rhs", "f32", shape = 10L)
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(integer(), integer())
  )
  f <- hlo_return(z)

  program <- pjrt::pjrt_program(repr(f))

  exec <- pjrt::pjrt_compile(program)
  x1 <- pjrt::pjrt_buffer(1:10, "f32")
  x2 <- pjrt::pjrt_buffer(2:11, "f32")
  out <- pjrt::pjrt_execute(exec, x1, x2)
  expect_equal(pjrt::as_array(out), outer(1:10, 2:11))
})
